(* Parser: token list -> AST.

   Grammar (stage A + operators + function types).

   Types: int, bool, CTOR[ty...], or fn(ty...) -> ty.

   Expressions parse with precedence climbing, lowest to highest:
     or-expr     [||]
     and-expr    [&&]
     eq-expr     [== !=]
     cmp-expr    [< > <= >=]
     add-expr    [+ -]
     mul-expr    [star slash percent]
     unary       [! -prefix]
     postfix     [chained applications]
     atom        [literal, var, ctor, parens, if, match]

   Operator desugaring: a + b parses to ECall(add, [a, b]) etc.
   Indirect application: f(x)(y) parses head f as EVar, applies first
   as ECall(f, [x]), and the second as EApply(ECall(...), [y]). *)

open Token
open Ast

exception Parse_error of string

type state = { mutable toks : token list }

let peek st = match st.toks with [] -> TEOF | t :: _ -> t
let advance st = match st.toks with [] -> () | _ :: ts -> st.toks <- ts
let eat st = let t = peek st in advance st; t
let expect st t =
  let got = peek st in
  if got = t then advance st
  else raise (Parse_error
    (Printf.sprintf "expected %s, got %s" (Token.show t) (Token.show got)))

(* ---------- types ---------- *)

let rec parse_ty st =
  match eat st with
  | TIntTy        -> TyInt
  | TBoolTy       -> TyBool
  | TByteTy       -> TyApp ("byte", [])
  | TFloatTy      -> TyApp ("float", [])
  | TStar         ->
      (* *T — raw C pointer. Prefix only; infix * is multiplication. *)
      let inner = parse_ty st in
      TyPtr inner
  | TFn           ->
      expect st TLParen;
      let args =
        if peek st = TRParen then []
        else parse_ty_list st
      in
      expect st TRParen;
      expect st TArrow;
      let ret = parse_ty st in
      TyFun (args, ret)
  | TCtorIdent s  ->
      if peek st = TLBracket then begin
        advance st;
        let args = parse_ty_list st in
        expect st TRBracket;
        TyApp (s, args)
      end else
        TyApp (s, [])
  | t -> raise (Parse_error
    (Printf.sprintf "expected type, got %s" (Token.show t)))

and parse_ty_list st =
  let first = parse_ty st in
  let rec rest () =
    if peek st = TComma then begin
      advance st;
      let t = parse_ty st in
      t :: rest ()
    end else []
  in
  first :: rest ()

(* ---------- type parameter lists ---------- *)

let parse_type_params st =
  if peek st = TLBracket then begin
    advance st;
    let rec collect () =
      match eat st with
      | TCtorIdent s ->
          if peek st = TComma then begin
            advance st;
            s :: collect ()
          end else
            [s]
      | t -> raise (Parse_error
        (Printf.sprintf "expected type parameter name, got %s"
           (Token.show t)))
    in
    let names = collect () in
    expect st TRBracket;
    names
  end else []

(* ---------- expressions: operator precedence ---------- *)

(* Helper: parse a chain of left-associative binary operators. Given
   a list of (token, binop) pairs, build EBinop nodes left-associatively. *)
let parse_binop_chain st (ops : (token * binop) list) lower =
  let lhs = lower st in
  let rec loop lhs =
    match
      List.find_opt (fun (tk, _) -> peek st = tk) ops
    with
    | Some (_, op) ->
        advance st;
        let rhs = lower st in
        loop (EBinop (op, lhs, rhs))
    | None -> lhs
  in
  loop lhs

let rec parse_expr st = parse_assign st

(* `:=` is allowed in two shapes:
     `a[i] := v` — array slot assignment (always allowed)
     `x := v`    — variable reassignment (requires `let mut x = ...`) *)
and parse_assign st =
  let lhs = parse_pipe st in
  if peek st = TColonEq then begin
    advance st;
    let rhs = parse_assign st in
    match lhs with
    | EIndex (arr, idx) -> EAssignIdx (arr, idx, rhs)
    | EVar x -> EAssign (x, rhs)
    | _ -> raise (Parse_error
        "`:=` requires a variable name or array indexing on the left")
  end else lhs

(* Pipeline: `x |> f` rewrites to `f(x)`. `x |> f(a, b)` rewrites to
   `f(x, a, b)` — x is threaded in as the FIRST argument. Left-
   associative: `x |> f |> g` is `g(f(x))`. Lower precedence than
   any binary operator, higher than assignment. *)
and parse_pipe st =
  let lhs = parse_or st in
  let rec loop lhs =
    if peek st = TPipeArrow then begin
      advance st;
      let rhs = parse_or st in
      let combined = match rhs with
        | ECall (f, args) -> ECall (f, lhs :: args)
        | _ -> ECall (rhs, [lhs])
      in
      loop combined
    end else lhs
  in
  loop lhs

(* Operator precedence, low → high (mirrors C so it's familiar):
     ||           — parse_or
     &&           — parse_and
     |            — parse_bor       (bitwise OR)
     ^            — parse_bxor      (bitwise XOR)
     &            — parse_band      (bitwise AND)
     == !=        — parse_eq
     < > <= >=    — parse_cmp
     << >>        — parse_shift
     + -          — parse_add
     * / %        — parse_mul
     ! - * ~      — parse_unary (prefix)
*)
and parse_or st =
  parse_binop_chain st [TOrOr, OpOr] parse_and

and parse_and st =
  parse_binop_chain st [TAndAnd, OpAnd] parse_bor

and parse_bor st =
  parse_binop_chain st [TPipe, OpBOr] parse_bxor

and parse_bxor st =
  parse_binop_chain st [TCaret, OpBXor] parse_band

and parse_band st =
  parse_binop_chain st [TAmp, OpBAnd] parse_eq

and parse_eq st =
  parse_binop_chain st [TEqEq, OpEq; TNeq, OpNeq] parse_cmp

and parse_cmp st =
  parse_binop_chain st
    [TLt, OpLt; TGt, OpGt; TLe, OpLe; TGe, OpGe]
    parse_shift

and parse_shift st =
  parse_binop_chain st [TShl, OpShl; TShr, OpShr] parse_add

and parse_add st =
  parse_binop_chain st [TPlus, OpAdd; TMinus, OpSub] parse_mul

and parse_mul st =
  parse_binop_chain st
    [TStar, OpMul; TSlash, OpDiv; TPercent, OpMod]
    parse_unary

and parse_unary st =
  match peek st with
  | TBang ->
      advance st;
      let inner = parse_unary st in
      EUnop (OpNot, inner)
  | TTilde ->
      advance st;
      let inner = parse_unary st in
      EUnop (OpBNot, inner)
  | TMinus ->
      advance st;
      let inner = parse_unary st in
      EUnop (OpNeg, inner)
  | TStar ->
      (* *p — pointer deref. Prefix-only here; infix * is matched in
         parse_mul, which only triggers after an operand has been parsed. *)
      advance st;
      let inner = parse_unary st in
      EDeref inner
  | TAwait ->
      advance st;
      parse_await_tail st
  | TSpawn ->
      advance st;
      let inner = parse_unary st in
      ESpawn inner
  | TYield ->
      advance st;
      EYield
  | _ -> parse_postfix st

(* After `await`: distinguish three forms based on look-ahead.
     await all { e1, e2, ... }   → EAwaitAll       (static, tuple result)
     await all <expr>            → EAwaitAllDyn    (dynamic, array result)
     await <expr>                → EAwait
   `all` is recognised here only — outside the `await` context it
   remains an ordinary identifier. *)
and parse_await_tail st =
  match peek st with
  | TIdent "all" ->
      advance st;
      (match peek st with
       | TLBrace ->
           advance st;
           let branches =
             if peek st = TRBrace then []
             else
               let rec collect () =
                 let e = parse_expr st in
                 if peek st = TComma then begin
                   advance st;
                   if peek st = TRBrace then [e] else e :: collect ()
                 end else [e]
               in
               collect ()
           in
           expect st TRBrace;
           EAwaitAll branches
       | _ ->
           let coll = parse_unary st in
           EAwaitAllDyn coll)
  | _ ->
      let inner = parse_unary st in
      EAwait inner

and parse_postfix st =
  let head = parse_atom st in
  parse_postfix_chain st head

and parse_postfix_chain st head =
  match peek st with
  | TLParen ->
      advance st;
      let args = parse_args st in
      expect st TRParen;
      parse_postfix_chain st (ECall (head, args))
  | TDot ->
      advance st;
      let field = match eat st with
        | TIdent s -> s
        | t -> raise (Parse_error
          (Printf.sprintf "expected field name after '.', got %s"
             (Token.show t)))
      in
      parse_postfix_chain st (EField (head, field))
  | TLBracket ->
      advance st;
      let idx = parse_expr st in
      expect st TRBracket;
      parse_postfix_chain st (EIndex (head, idx))
  | _ -> head

and parse_record_init_elems st =
  if peek st = TRBrace then []
  else begin
    let e = parse_record_init_elem st in
    match peek st with
    | TComma -> advance st;
        if peek st = TRBrace then [e]
        else e :: parse_record_init_elems st
    | _      -> [e]
  end

and parse_record_init_elem st =
  if peek st = TDotDot then begin
    advance st;
    let base = parse_expr st in
    RSpread base
  end else begin
    let name = match eat st with
      | TIdent s -> s
      | t -> raise (Parse_error
        (Printf.sprintf "expected field name or `..spread`, got %s"
           (Token.show t)))
    in
    expect st TColon;
    let value = parse_expr st in
    RAssign (name, value)
  end

and parse_args st =
  if peek st = TRParen then []
  else begin
    let first = parse_expr st in
    let rec rest () =
      if peek st = TComma then begin
        advance st;
        let e = parse_expr st in
        e :: rest ()
      end else []
    in
    first :: rest ()
  end

and parse_atom st =
  match peek st with
  | TInt _ | TFloat _ | TTrue | TFalse | TLParen | TLBrace
  | TIdent _ | TCtorIdent _
  | TStringLit _
  | TIf | TMatch | TWhile | TBreak | TContinue | TFor | TReturn
  | TArray | TLen | TSlice
  | TToInt | TToByte | TToFloat
  | TCAlloc | TCFree | TNullPtr | TIsNull | TArrayData | TTryAt | TDrop
  | TRegion | TStackRegion | TAlignedRegion -> parse_atom_consume st
  | t -> raise (Parse_error
    (Printf.sprintf "expected expression, got %s" (Token.show t)))

and parse_atom_consume st =
  match peek st with
  | TLBrace ->
      (* Bare `{ ... }` as an expression — a block. Useful in match
         arm bodies where you want let-bindings before the result. *)
      parse_block st
  | _ ->
  match eat st with
  | TInt n      -> EInt n
  | TFloat f    -> EFloat f
  | TTrue       -> EBool true
  | TFalse      -> EBool false
  | TStringLit s -> EStringLit s
  | TLParen     ->
      let e = parse_expr st in
      expect st TRParen;
      e
  | TIdent name -> EVar name
  | TCtorIdent name ->
      (match peek st with
       | TLParen ->
           advance st;
           let args = parse_args st in
           expect st TRParen;
           ECtor (name, args)
       | TLBrace ->
           advance st;
           let elems = parse_record_init_elems st in
           expect st TRBrace;
           ERecord (name, elems)
       | _ -> ECtor (name, []))
  | TIf -> parse_if_after_kw st
  | TMatch -> parse_match_after_kw st
  | TWhile ->
      let cond = parse_expr st in
      let body = parse_block st in
      EWhile (cond, body)
  | TBreak -> EBreak
  | TContinue -> EContinue
  | TFor ->
      (* Two shapes share the `for x in ...` head:
           for <var> in <lo>..<hi> { <body> }   — int range, desugars to while.
           for <var> in <stream-expr> { <body> } — multishot Stream[T] drain.
         Parse a single expression after `in` and disambiguate on the
         next token: `..` selects the range path, `{` selects the
         stream path. *)
      let var = match eat st with
        | TIdent s -> s
        | t -> raise (Parse_error
          (Printf.sprintf "expected loop variable name after `for`, got %s"
             (Token.show t)))
      in
      expect st TIn;
      let lo_or_src = parse_expr st in
      (match peek st with
       | TDotDot ->
           advance st;
           let hi = parse_expr st in
           let body = parse_block st in
           let hi_var = Printf.sprintf "_for_hi_%d" (Hashtbl.hash (var, hi)) in
           let bump = EAssign (var, EBinop (OpAdd, EVar var, EInt 1)) in
           let new_body =
             ELet ("_", false, None, body,
               ELet ("_", false, None, bump, EInt 0))
           in
           ELet (hi_var, false, None, hi,
             ELet (var, true, None, lo_or_src,
               EWhile (EBinop (OpLt, EVar var, EVar hi_var), new_body)))
       | TLBrace ->
           let body = parse_block st in
           EForStream (var, lo_or_src, body)
       | t ->
           raise (Parse_error
             (Printf.sprintf
                "after `for %s in <expr>`: expected `..` (range form) or `{` (stream form), got %s"
                var (Token.show t))))
  | TReturn ->
      let v = parse_expr st in
      EReturn v
  | TArray ->
      expect st TLParen;
      let r = parse_expr st in
      expect st TComma;
      (match peek st with
       | TLBracket ->
           (* array(r, [v0, v1, ..., vN]) — initialize from literal. *)
           advance st;
           let elems =
             if peek st = TRBracket then []
             else
               let rec collect () =
                 let e = parse_expr st in
                 if peek st = TComma then begin
                   advance st;
                   if peek st = TRBracket then [e] else e :: collect ()
                 end else [e]
               in
               collect ()
           in
           expect st TRBracket;
           expect st TRParen;
           EArrayLit (r, elems)
       | _ ->
           let n = parse_expr st in
           expect st TComma;
           let v = parse_expr st in
           expect st TRParen;
           EArray (r, n, v))
  | TRegion ->
      expect st TLParen;
      let n = parse_expr st in
      expect st TRParen;
      ERegion n
  | TLen ->
      expect st TLParen;
      let e = parse_expr st in
      expect st TRParen;
      ELen e
  | TSlice ->
      expect st TLParen;
      let a = parse_expr st in
      expect st TComma;
      let lo = parse_expr st in
      expect st TComma;
      let hi = parse_expr st in
      expect st TRParen;
      ESlice (a, lo, hi)
  | TToInt ->
      expect st TLParen;
      let e = parse_expr st in
      expect st TRParen;
      EToInt e
  | TToByte ->
      expect st TLParen;
      let e = parse_expr st in
      expect st TRParen;
      EToByte e
  | TToFloat ->
      expect st TLParen;
      let e = parse_expr st in
      expect st TRParen;
      EToFloat e
  | TCAlloc ->
      expect st TLBracket;
      let t = parse_ty st in
      expect st TRBracket;
      expect st TLParen;
      let n = parse_expr st in
      expect st TRParen;
      ECAlloc (t, n)
  | TCFree ->
      expect st TLParen;
      let p = parse_expr st in
      expect st TRParen;
      ECFree p
  | TNullPtr ->
      expect st TLBracket;
      let t = parse_ty st in
      expect st TRBracket;
      expect st TLParen;
      expect st TRParen;
      ENullPtr t
  | TIsNull ->
      expect st TLParen;
      let p = parse_expr st in
      expect st TRParen;
      EIsNull p
  | TArrayData ->
      expect st TLParen;
      let a = parse_expr st in
      expect st TRParen;
      EArrayData a
  | TTryAt ->
      expect st TLParen;
      let a = parse_expr st in
      expect st TComma;
      let i = parse_expr st in
      expect st TRParen;
      ETryAt (a, i)
  | TDrop ->
      expect st TLParen;
      let e = parse_expr st in
      expect st TRParen;
      EDrop e
  | TStackRegion ->
      (* stack_region(N) — N must be an int literal (compile-time size).
         The block lives in the surrounding C function's frame; the
         slot still goes through the slab so gen-check still works. *)
      expect st TLParen;
      let n = parse_expr st in
      (match n with
       | EInt _ -> ()
       | _ -> raise (Parse_error
           "stack_region(N): N must be an integer literal"));
      expect st TRParen;
      EStackRegion n
  | TAlignedRegion ->
      (* aligned_region(N, A) — N is the size in bytes, A is the
         alignment (must be an int literal and a power of two). *)
      expect st TLParen;
      let n = parse_expr st in
      expect st TComma;
      let a = parse_expr st in
      (match a with
       | EInt k when k > 0 && (k land (k - 1)) = 0 -> ()
       | EInt _ -> raise (Parse_error
           "aligned_region(_, A): A must be a positive power of two")
       | _ -> raise (Parse_error
           "aligned_region(_, A): A must be an integer literal"));
      expect st TRParen;
      EAlignedRegion (n, a)
  | _ -> assert false

and parse_if_after_kw st =
  let cond = parse_expr st in
  let then_b = parse_block st in
  (* `else` is optional. When absent, the implicit else is int 0 —
     both branches must then unify to int. This is the form used
     inside loops: `if cond { break }`. *)
  if peek st <> TElse then
    EIf (cond, then_b, EInt 0)
  else begin
    advance st;
    (* Support `else if ... { ... }` as sugar for nested if. *)
    let else_b =
      if peek st = TIf then begin
        advance st;
        parse_if_after_kw st
      end else
        parse_block st
    in
    EIf (cond, then_b, else_b)
  end

and parse_match_after_kw st =
  let scrut = parse_expr st in
  expect st TLBrace;
  let arms = parse_arms st in
  expect st TRBrace;
  EMatch (scrut, arms)

and parse_arms st =
  if peek st = TRBrace then []
  else begin
    let arm = parse_arm st in
    match peek st with
    | TComma -> advance st; arm :: parse_arms st
    | _      -> [arm]
  end

and parse_arm st =
  let p = parse_pat st in
  let guard =
    if peek st = TIf then begin
      advance st;
      Some (parse_expr st)
    end else None
  in
  expect st TFatArrow;
  let body = parse_expr st in
  (p, guard, body)

and parse_pat st =
  let first = parse_single_pat st in
  if peek st = TPipe then begin
    let rec collect acc =
      if peek st = TPipe then begin
        advance st;
        let p = parse_single_pat st in
        collect (p :: acc)
      end else
        List.rev acc
    in
    POr (first :: collect [])
  end else first

and parse_single_pat st =
  match eat st with
  | TUnderscore  -> PBind "_"
  | TCtorIdent c ->
      if peek st = TLParen then begin
        advance st;
        let vars = parse_pat_vars st in
        expect st TRParen;
        PCtor (c, vars)
      end else
        PCtor (c, [])
  | TIdent x -> PBind x
  | TInt n -> PInt n
  | TMinus ->
      (match eat st with
       | TInt n -> PInt (- n)
       | t -> raise (Parse_error
         (Printf.sprintf "expected integer literal after `-` in pattern, got %s"
            (Token.show t))))
  | TTrue  -> PBool true
  | TFalse -> PBool false
  | TStringLit s -> PStr s
  | t -> raise (Parse_error
    (Printf.sprintf "expected pattern, got %s" (Token.show t)))

and parse_pat_vars st =
  if peek st = TRParen then []
  else begin
    let v = parse_pat_var st in
    match peek st with
    | TComma -> advance st; v :: parse_pat_vars st
    | _      -> [v]
  end

and parse_pat_var st =
  match eat st with
  | TIdent s    -> s
  | TUnderscore -> "_"
  | t           -> raise (Parse_error
    (Printf.sprintf "expected variable or '_' in pattern, got %s"
       (Token.show t)))

and parse_block st =
  expect st TLBrace;
  let body = parse_block_body st in
  expect st TRBrace;
  body

and parse_block_body st =
  match peek st with
  | TLet ->
      advance st;
      let is_mut =
        if peek st = TMut then begin advance st; true end else false
      in
      let name = match eat st with
        | TIdent s    -> s
        | TUnderscore -> "_"
        | t -> raise (Parse_error
          (Printf.sprintf "expected identifier after 'let', got %s"
            (Token.show t)))
      in
      let ascription =
        if peek st = TColon then begin
          advance st;
          Some (parse_ty st)
        end else None
      in
      expect st TEq;
      let value = parse_expr st in
      expect st TSemi;
      let body = parse_block_body st in
      ELet (name, is_mut, ascription, value, body)
  | _ ->
      (* Whether the upcoming expression starts with a `{...}`-bearing
         keyword. If so, an implicit `;` is allowed after the closing
         `}` (the for-desugar wraps the EWhile in lets, so peeking AFTER
         parse_expr can't see this — we peek BEFORE). *)
      let starts_block_like = match peek st with
        | TIf | TMatch | TWhile | TFor -> true
        | _ -> false
      in
      let e = parse_expr st in
      if peek st = TSemi then begin
        advance st;
        if peek st = TRBrace then
          (* Trailing `;` discards the last expression's value; the
             block's result becomes int 0 (placeholder for unit). *)
          ELet ("_", false, None, e, EInt 0)
        else
          let rest = parse_block_body st in
          ELet ("_", false, None, e, rest)
      end
      else if starts_block_like && peek st <> TRBrace then
        (* Block-like construct followed by another statement without a
           `;` between — implicit boundary, treat as discard. *)
        let rest = parse_block_body st in
        ELet ("_", false, None, e, rest)
      else e

(* ---------- functions ---------- *)

let parse_param st =
  let name = match eat st with
    | TIdent s -> s
    | t -> raise (Parse_error
      (Printf.sprintf "expected parameter name, got %s" (Token.show t)))
  in
  expect st TColon;
  let ty = parse_ty st in
  (name, ty)

let rec parse_params_rest st =
  if peek st = TComma then begin
    advance st;
    let p = parse_param st in
    p :: parse_params_rest st
  end else []

let parse_params st =
  if peek st = TRParen then []
  else
    let p = parse_param st in
    p :: parse_params_rest st

let parse_func st =
  expect st TFn;
  let name = match eat st with
    | TIdent s -> s
    | t -> raise (Parse_error
      (Printf.sprintf "expected function name, got %s" (Token.show t)))
  in
  let type_params = parse_type_params st in
  expect st TLParen;
  let params = parse_params st in
  expect st TRParen;
  expect st TArrow;
  let return_ty = parse_ty st in
  let body = parse_block st in
  { name; type_params; params; return_ty; body }

let parse_extern st =
  expect st TExtern;
  let is_async =
    if peek st = TAsync then (advance st; true) else false
  in
  (* `extern async stream fn ...` — multishot variant. `stream` is
     only meaningful after `async`; reject `extern stream fn ...`. *)
  let is_stream =
    if peek st = TStream then begin
      if not is_async then
        raise (Parse_error
          "`extern stream fn ...` requires `async`: write `extern async stream fn ...`");
      advance st; true
    end else false
  in
  expect st TFn;
  let name = match eat st with
    | TIdent s -> s
    | t -> raise (Parse_error
      (Printf.sprintf "expected extern function name, got %s"
         (Token.show t)))
  in
  expect st TLParen;
  let params = parse_params st in
  expect st TRParen;
  expect st TArrow;
  let return_ty = parse_ty st in
  { ext_name = name; ext_params = params; ext_return_ty = return_ty;
    ext_is_async = is_async; ext_is_stream = is_stream }

(* ---------- type declarations ---------- *)

let parse_struct st ~is_linear : top_decl =
  expect st TStruct;
  let name = match eat st with
    | TCtorIdent s -> s
    | t -> raise (Parse_error
      (Printf.sprintf "expected type name after `struct`, got %s"
         (Token.show t)))
  in
  let type_params = parse_type_params st in
  expect st TLBrace;
  let rec collect_fields () =
    if peek st = TRBrace then []
    else
      let fname = match eat st with
        | TIdent s -> s
        | t -> raise (Parse_error
          (Printf.sprintf "expected field name, got %s" (Token.show t)))
      in
      expect st TColon;
      let fty = parse_ty st in
      let rest =
        if peek st = TComma then begin
          advance st;
          collect_fields ()
        end else []
      in
      (fname, fty) :: rest
  in
  let fields = collect_fields () in
  expect st TRBrace;
  TopRecord {
    rec_name = name;
    rec_type_params = type_params;
    rec_fields = fields;
    rec_is_linear = is_linear;
  }

let parse_enum st ~is_linear : top_decl =
  expect st TEnum;
  let name = match eat st with
    | TCtorIdent s -> s
    | t -> raise (Parse_error
      (Printf.sprintf "expected type name after `enum`, got %s"
         (Token.show t)))
  in
  let type_params = parse_type_params st in
  expect st TLBrace;
  let rec collect_variants () =
    if peek st = TRBrace then []
    else
      let cname = match eat st with
        | TCtorIdent s -> s
        | t -> raise (Parse_error
          (Printf.sprintf "expected variant constructor (CapitalCase), got %s"
            (Token.show t)))
      in
      let arg_tys =
        if peek st = TLParen then begin
          advance st;
          let tys = parse_ty_list st in
          expect st TRParen;
          tys
        end else []
      in
      let v = { ctor_name = cname; arg_tys } in
      let rest =
        if peek st = TComma then begin
          advance st;
          collect_variants ()
        end else []
      in
      v :: rest
  in
  let variants = collect_variants () in
  expect st TRBrace;
  TopType { type_name = name; type_params; variants; is_linear }

(* ---------- use declarations ---------- *)

(* `use foo::bar;` or `use foo::{a, b, c};` — selective import. *)
let parse_use st : use_decl =
  expect st TUse;
  let module_name = match eat st with
    | TIdent s -> s
    | t -> raise (Parse_error
      (Printf.sprintf "expected module name after `use`, got %s"
         (Token.show t)))
  in
  expect st TColonCol;
  let items =
    if peek st = TLBrace then begin
      advance st;
      let rec collect () =
        let item = match eat st with
          | TIdent s     -> s
          | TCtorIdent s -> s
          | t -> raise (Parse_error
            (Printf.sprintf "expected import item, got %s" (Token.show t)))
        in
        if peek st = TComma then begin
          advance st;
          if peek st = TRBrace then [item]
          else item :: collect ()
        end else [item]
      in
      let items = collect () in
      expect st TRBrace;
      items
    end else
      let item = match eat st with
        | TIdent s     -> s
        | TCtorIdent s -> s
        | t -> raise (Parse_error
          (Printf.sprintf "expected import item, got %s" (Token.show t)))
      in
      [item]
  in
  expect st TSemi;
  if items = [] then
    raise (Parse_error
      (Printf.sprintf "use %s::{} — must import at least one item" module_name));
  { use_module = module_name; use_items = items }

(* ---------- entry point ---------- *)

let parse (toks : token list) : program =
  let st = { toks } in
  let rec loop acc =
    match peek st with
    | TEOF  -> List.rev acc
    | TUse ->
        let u = parse_use st in
        loop (TopUse u :: acc)
    | TFn   ->
        let f = parse_func st in
        loop (TopFunc f :: acc)
    | TStruct ->
        let td = parse_struct st ~is_linear:false in
        loop (td :: acc)
    | TEnum ->
        let td = parse_enum st ~is_linear:false in
        loop (td :: acc)
    | TLinear ->
        advance st;
        (match peek st with
         | TStruct ->
             let td = parse_struct st ~is_linear:true in
             loop (td :: acc)
         | TEnum ->
             let td = parse_enum st ~is_linear:true in
             loop (td :: acc)
         | t -> raise (Parse_error
           (Printf.sprintf "expected `struct` or `enum` after `linear`, got %s"
              (Token.show t))))
    | TType ->
        advance st;
        let name = match eat st with
          | TCtorIdent s -> s
          | t -> raise (Parse_error
            (Printf.sprintf "expected type alias name after `type`, got %s"
               (Token.show t)))
        in
        expect st TEq;
        let target = parse_ty st in
        expect st TSemi;
        loop (TopAlias { alias_name = name; alias_ty = target } :: acc)
    | TExtern ->
        let e = parse_extern st in
        loop (TopExtern e :: acc)
    | t -> raise (Parse_error
      (Printf.sprintf "expected `use`, `fn`, `struct`, `enum`, `linear`, `type`, or `extern` at top level, got %s"
         (Token.show t)))
  in
  loop []

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

(* `:=` is right-associative: `a := b := c` parses as `a := (b := c)`.
   Returns Option[T] — but that's purely a type-system fact handled in
   the checker. Lowest precedence. *)
and parse_assign st =
  let lhs = parse_orelse st in
  if peek st = TColonEq then begin
    advance st;
    let rhs = parse_assign st in
    match lhs with
    | EIndex (arr, idx) -> EAssignIdx (arr, idx, rhs)
    | _                 -> EAssign (lhs, rhs)
  end else lhs

(* `??` is right-associative: `a ?? b ?? c` parses as `a ?? (b ?? c)`.
   That's the useful form: cascade of defaults. *)
and parse_orelse st =
  let lhs = parse_or st in
  if peek st = TQQ then begin
    advance st;
    let rhs = parse_orelse st in
    EOrElse (lhs, rhs)
  end else lhs

and parse_or st =
  parse_binop_chain st [TOrOr, OpOr] parse_and

and parse_and st =
  parse_binop_chain st [TAndAnd, OpAnd] parse_eq

and parse_eq st =
  parse_binop_chain st [TEqEq, OpEq; TNeq, OpNeq] parse_cmp

and parse_cmp st =
  parse_binop_chain st
    [TLt, OpLt; TGt, OpGt; TLe, OpLe; TGe, OpGe]
    parse_add

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
  | TMinus ->
      advance st;
      let inner = parse_unary st in
      EUnop (OpNeg, inner)
  | _ -> parse_postfix st

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
  | TInt _ | TTrue | TFalse | TLParen
  | TIdent _ | TCtorIdent _
  | TIf | TMatch
  | TRef | TDeref | TPanic
  | TOwn | TTake | TUnwrap | TLook
  | TArray | TLen | TRegion -> parse_atom_consume st
  | t -> raise (Parse_error
    (Printf.sprintf "expected expression, got %s" (Token.show t)))

and parse_atom_consume st =
  match eat st with
  | TInt n      -> EInt n
  | TTrue       -> EBool true
  | TFalse      -> EBool false
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
  | TRef ->
      expect st TLParen;
      let e = parse_expr st in
      expect st TRParen;
      ERef e
  | TDeref ->
      expect st TLParen;
      let e = parse_expr st in
      expect st TRParen;
      EDeref e
  | TPanic ->
      expect st TLParen;
      expect st TRParen;
      EPanic
  | TOwn ->
      expect st TLParen;
      let e = parse_expr st in
      expect st TRParen;
      EOwn e
  | TTake ->
      expect st TLParen;
      let e = parse_expr st in
      expect st TRParen;
      ETake e
  | TUnwrap ->
      expect st TLParen;
      let e = parse_expr st in
      expect st TRParen;
      EUnwrap e
  | TLook ->
      expect st TLParen;
      let e = parse_expr st in
      expect st TRParen;
      ELook e
  | TArray ->
      expect st TLParen;
      let r = parse_expr st in
      expect st TComma;
      let n = parse_expr st in
      expect st TComma;
      let v = parse_expr st in
      expect st TRParen;
      EArray (r, n, v)
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
  | _ -> assert false

and parse_if_after_kw st =
  let cond = parse_expr st in
  let then_b = parse_block st in
  expect st TElse;
  let else_b = parse_block st in
  EIf (cond, then_b, else_b)

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
  expect st TFatArrow;
  let body = parse_expr st in
  (p, body)

and parse_pat st =
  match eat st with
  | TUnderscore  -> PWild
  | TCtorIdent c ->
      if peek st = TLParen then begin
        advance st;
        let vars = parse_pat_vars st in
        expect st TRParen;
        PCtor (c, vars)
      end else
        PCtor (c, [])
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
      ELet (name, ascription, value, body)
  | _ ->
      let e = parse_expr st in
      if peek st = TSemi then begin
        advance st;
        if peek st = TRBrace then
          (* trailing `;` before `}` — treat as expression-with-unit-result.
             We have no unit, so allow this only if the expression is the
             last thing and just discard the trailing semi. But since we
             must produce SOME value as block result, this is an error. *)
          raise (Parse_error
            "block cannot end with `;` — last expression is the block's value")
        else
          let rest = parse_block_body st in
          ELet ("_", None, e, rest)
      end else e

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
  { ext_name = name; ext_params = params; ext_return_ty = return_ty }

(* ---------- type declarations ---------- *)

let parse_struct st : top_decl =
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
  }

let parse_enum st : top_decl =
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
  TopType { type_name = name; type_params; variants }

(* ---------- entry point ---------- *)

let parse (toks : token list) : program =
  let st = { toks } in
  let rec loop acc =
    match peek st with
    | TEOF  -> List.rev acc
    | TFn   ->
        let f = parse_func st in
        loop (TopFunc f :: acc)
    | TStruct ->
        let td = parse_struct st in
        loop (td :: acc)
    | TEnum ->
        let td = parse_enum st in
        loop (td :: acc)
    | TType ->
        raise (Parse_error
          "`type` keyword is reserved for future aliases; use `struct` or `enum`")
    | TExtern ->
        let e = parse_extern st in
        loop (TopExtern e :: acc)
    | t -> raise (Parse_error
      (Printf.sprintf "expected `fn`, `struct`, `enum`, or `extern` at top level, got %s"
         (Token.show t)))
  in
  loop []

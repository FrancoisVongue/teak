(* AST for stage A.

   New since stage B:
   - ty: now includes TyVar (bound type variable in a definition,
       e.g. T inside `fn id[T]`) and TyApp (applied type with a
       list of type arguments, e.g. Option[int]).
       TyMeta is also here but only the checker generates those —
       parser will never produce TyMeta.
   - type_decl, func: gained `type_params: string list`.
   - ELet: gained optional type ascription (`let x: ty = ...`).

   `TyUser n` from stage B is now expressible as `TyApp (n, [])`. *)

type meta = {
  id              : int;
  mutable resolved: ty option;
}

and ty =
  | TyInt
  | TyBool
  | TyVar  of string             (* rigid, declared in [T] of a fn/type decl *)
  | TyApp  of string * ty list   (* e.g. Option[int], Shape (= TyApp("Shape", [])) *)
  | TyFun  of ty list * ty       (* fn(args) -> ret *)
  | TyPtr  of ty                 (* *T — raw C pointer, escape hatch for FFI *)
  | TyMeta of meta               (* unification variable, only inside the checker *)

type pat =
  | PBind of string                      (* lowercase ident — binds scrutinee to name; "_" = wildcard *)
  | PCtor of string * string list
  | POr   of pat list                    (* a | b | c — all must be PCtor or literals, no bindings *)
  | PInt  of int                         (* literal int pattern *)
  | PBool of bool                        (* literal bool pattern *)
  | PStr  of string                      (* literal Array[byte] pattern *)

type binop =
  | OpAdd | OpSub | OpMul | OpDiv | OpMod
  | OpEq  | OpNeq
  | OpLt  | OpGt  | OpLe  | OpGe
  | OpAnd | OpOr
  | OpBOr | OpBAnd | OpBXor             (* bitwise on int *)
  | OpShl | OpShr                       (* shifts on int *)

type unop = OpNeg | OpNot | OpBNot

type expr =
  | EInt    of int
  | EFloat  of float
  | EBool   of bool
  | EVar    of string
  | EStringLit of string                  (* "..." — byte literal in static region *)
  | EBinop  of binop * expr * expr
  | EUnop   of unop  * expr
  | ECall   of expr * expr list
  | ECtor   of string * expr list
  | ERecord of string * record_init_elem list
  | EField  of expr * string
  | EIf     of expr * expr * expr
  | ELet    of string * bool * ty option * expr * expr
                                          (* name, mut?, optional ascription, value, body *)
  | EAssign of string * expr              (* x := v — requires x to be mut *)
  | EWhile  of expr * expr                (* while cond { body } — result is int 0 *)
  | EBreak                                (* break;    — valid only inside while *)
  | EContinue                             (* continue; — valid only inside while *)
  | EReturn of expr                       (* return v; — early exit from enclosing fn *)
  | EMatch  of expr * (pat * expr option * expr) list
                                          (* (pattern, optional `if guard`, body) *)
  | EArray  of expr * expr * expr         (* array(r, N, init) — allocate N slots in region r *)
  | EArrayLit of expr * expr list         (* array(r, [v0, v1, ...]) — allocate and initialize *)
  | ERegion of expr                       (* region(N) — heap arena, malloc'd block *)
  | EStackRegion of expr                  (* stack_region(N) — N literal, block on stack *)
  | EAlignedRegion of expr * expr         (* aligned_region(N, A) — heap, A-byte aligned *)
  | EIndex  of expr * expr                (* a[i] — read element *)
  | EAssignIdx of expr * expr * expr      (* a[i] := v — write element, returns int *)
  | ELen    of expr                       (* len(a) — array length *)
  | ESlice  of expr * expr * expr         (* slice(a, lo, hi) — sub-handle in same region *)
  | EToInt  of expr                       (* to_int(b|f) — byte→int or float→int truncate *)
  | EToByte of expr                       (* to_byte(n) — truncate int to byte *)
  | EToFloat of expr                      (* to_float(n) — int → float *)
  | ECAlloc of ty * expr                  (* c_alloc[T](n) — malloc n*sizeof(T), returns *T *)
  | ECFree  of expr                       (* c_free(p) — free raw pointer *)
  | ENullPtr of ty                        (* null_ptr[T]() — typed NULL *)
  | EIsNull of expr                       (* is_null(p) — NULL check *)
  | EArrayData of expr                    (* array_data(a) — *T view of Array[T] bytes *)
  | ETryAt  of expr * expr                (* try_at(a, i) — None on dangling/oob *)
  | EDrop   of expr                       (* drop(x) — consume linear value, run its drop fn *)
  | EDeref  of expr                       (* *p — pointer deref *)

and record_init_elem =
  | RAssign of string * expr   (* field: value *)
  | RSpread of expr            (* ..base *)

type variant = {
  ctor_name : string;
  arg_tys   : ty list;
}

type type_decl = {
  type_name   : string;
  type_params : string list;
  variants    : variant list;
  is_linear   : bool;   (* `linear enum Foo { ... }` — move-only with user drop fn *)
}

type record_decl = {
  rec_name        : string;
  rec_type_params : string list;
  rec_fields      : (string * ty) list;
  rec_is_linear   : bool;   (* `linear struct Foo { ... }` *)
}

type func = {
  name        : string;
  type_params : string list;
  params      : (string * ty) list;
  return_ty   : ty;
  body        : expr;
}

type extern_decl = {
  ext_name      : string;
  ext_params    : (string * ty) list;
  ext_return_ty : ty;
}

(* `use mod::{a, b, c};` — selective import from another module.
   Items must be a non-empty list; bare `use mod;` is not supported. *)
type use_decl = {
  use_module : string;
  use_items  : string list;
}

(* `type Bytes = Array[byte];` — a plain alias. Resolved away by the
   resolver before type checking; no runtime presence. Non-generic only. *)
type alias_decl = {
  alias_name : string;
  alias_ty   : ty;
}

type top_decl =
  | TopType   of type_decl
  | TopRecord of record_decl
  | TopFunc   of func
  | TopExtern of extern_decl
  | TopUse    of use_decl
  | TopAlias  of alias_decl

type program = top_decl list

(* ---------- pretty printers (for debugging only) ---------- *)

let rec show_ty = function
  | TyInt          -> "int"
  | TyBool         -> "bool"
  | TyVar n        -> n
  | TyApp (n, [])  -> n
  | TyApp (n, args) ->
      Printf.sprintf "%s[%s]" n
        (String.concat ", " (List.map show_ty args))
  | TyFun (args, ret) ->
      Printf.sprintf "fn(%s) -> %s"
        (String.concat ", " (List.map show_ty args))
        (show_ty ret)
  | TyPtr t -> "*" ^ show_ty t
  | TyMeta { resolved = Some t; _ } -> show_ty t
  | TyMeta { id; resolved = None } -> Printf.sprintf "?%d" id

let rec show_pat = function
  | PBind x            -> x       (* "_" is the wildcard *)
  | PCtor (c, [])      -> c
  | PCtor (c, vs)      ->
      Printf.sprintf "%s(%s)" c (String.concat ", " vs)
  | POr pats ->
      String.concat " | " (List.map show_pat pats)
  | PInt n  -> string_of_int n
  | PBool b -> if b then "true" else "false"
  | PStr s  -> Printf.sprintf "%S" s

let show_binop = function
  | OpAdd -> "+"  | OpSub -> "-"
  | OpMul -> "*"  | OpDiv -> "/"  | OpMod -> "%"
  | OpEq  -> "==" | OpNeq -> "!="
  | OpLt  -> "<"  | OpGt  -> ">"
  | OpLe  -> "<=" | OpGe  -> ">="
  | OpAnd -> "&&" | OpOr  -> "||"
  | OpBOr -> "|"  | OpBAnd -> "&" | OpBXor -> "^"
  | OpShl -> "<<" | OpShr -> ">>"

let show_unop = function
  | OpNeg  -> "-"
  | OpNot  -> "!"
  | OpBNot -> "~"

let rec show_expr = function
  | EInt n          -> string_of_int n
  | EFloat f        -> Printf.sprintf "%g" f
  | EBool true      -> "true"
  | EBool false     -> "false"
  | EVar x          -> x
  | EStringLit s    -> Printf.sprintf "%S" s
  | EBinop (op, a, b) ->
      Printf.sprintf "(%s %s %s)"
        (show_expr a) (show_binop op) (show_expr b)
  | EUnop (op, e) ->
      Printf.sprintf "%s%s" (show_unop op) (show_expr e)
  | ECall (f, args) ->
      Printf.sprintf "(%s)(%s)"
        (show_expr f)
        (String.concat ", " (List.map show_expr args))
  | ECtor (c, []) -> c
  | ECtor (c, args) ->
      Printf.sprintf "%s(%s)" c
        (String.concat ", " (List.map show_expr args))
  | ERecord (n, elems) ->
      let parts = List.map (function
        | RAssign (f, e) -> Printf.sprintf "%s: %s" f (show_expr e)
        | RSpread e      -> Printf.sprintf "..%s" (show_expr e)
      ) elems in
      Printf.sprintf "%s { %s }" n (String.concat ", " parts)
  | EField (e, f) ->
      Printf.sprintf "%s.%s" (show_expr e) f
  | EIf (c, t, e)   ->
      Printf.sprintf "if %s { %s } else { %s }"
        (show_expr c) (show_expr t) (show_expr e)
  | ELet (x, m, None, v, b)  ->
      Printf.sprintf "let %s%s = %s; %s"
        (if m then "mut " else "") x (show_expr v) (show_expr b)
  | ELet (x, m, Some ty, v, b)  ->
      Printf.sprintf "let %s%s: %s = %s; %s"
        (if m then "mut " else "") x (show_ty ty) (show_expr v) (show_expr b)
  | EAssign (x, v) -> Printf.sprintf "(%s := %s)" x (show_expr v)
  | EWhile (c, b) -> Printf.sprintf "while %s { %s }" (show_expr c) (show_expr b)
  | EBreak    -> "break"
  | EContinue -> "continue"
  | EReturn e -> Printf.sprintf "return %s" (show_expr e)
  | EMatch (e, arms) ->
      let arm_strs = List.map (fun (p, guard, body) ->
        let g = match guard with
          | None -> ""
          | Some g -> Printf.sprintf " if %s" (show_expr g)
        in
        Printf.sprintf "%s%s => %s"
          (show_pat p) g (show_expr body)) arms
      in
      Printf.sprintf "match %s { %s }"
        (show_expr e) (String.concat ", " arm_strs)
  | EArray (r, n, v) ->
      Printf.sprintf "array(%s, %s, %s)" (show_expr r) (show_expr n) (show_expr v)
  | EArrayLit (r, elems) ->
      Printf.sprintf "array(%s, [%s])" (show_expr r)
        (String.concat ", " (List.map show_expr elems))
  | EStackRegion n -> Printf.sprintf "stack_region(%s)" (show_expr n)
  | EAlignedRegion (n, a) ->
      Printf.sprintf "aligned_region(%s, %s)" (show_expr n) (show_expr a)
  | ERegion n -> Printf.sprintf "region(%s)" (show_expr n)
  | EIndex (a, i) -> Printf.sprintf "%s[%s]" (show_expr a) (show_expr i)
  | EAssignIdx (a, i, v) ->
      Printf.sprintf "(%s[%s] := %s)" (show_expr a) (show_expr i) (show_expr v)
  | ELen e -> Printf.sprintf "len(%s)" (show_expr e)
  | ESlice (a, lo, hi) ->
      Printf.sprintf "slice(%s, %s, %s)"
        (show_expr a) (show_expr lo) (show_expr hi)
  | EToInt e  -> Printf.sprintf "to_int(%s)"  (show_expr e)
  | EToByte e -> Printf.sprintf "to_byte(%s)" (show_expr e)
  | EToFloat e -> Printf.sprintf "to_float(%s)" (show_expr e)
  | ECAlloc (t, n) ->
      Printf.sprintf "c_alloc[%s](%s)" (show_ty t) (show_expr n)
  | ECFree p -> Printf.sprintf "c_free(%s)" (show_expr p)
  | ENullPtr t -> Printf.sprintf "null_ptr[%s]()" (show_ty t)
  | EIsNull p -> Printf.sprintf "is_null(%s)" (show_expr p)
  | EArrayData a -> Printf.sprintf "array_data(%s)" (show_expr a)
  | ETryAt (a, i) -> Printf.sprintf "try_at(%s, %s)" (show_expr a) (show_expr i)
  | EDrop e -> Printf.sprintf "drop(%s)" (show_expr e)
  | EDeref p -> Printf.sprintf "*%s" (show_expr p)

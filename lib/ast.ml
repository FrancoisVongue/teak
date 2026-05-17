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
  | TyMeta of meta               (* unification variable, only inside the checker *)

type pat =
  | PWild
  | PCtor of string * string list

type binop =
  | OpAdd | OpSub | OpMul | OpDiv | OpMod
  | OpEq  | OpNeq
  | OpLt  | OpGt  | OpLe  | OpGe
  | OpAnd | OpOr

type unop = OpNeg | OpNot

type expr =
  | EInt    of int
  | EBool   of bool
  | EVar    of string
  | EBinop  of binop * expr * expr
  | EUnop   of unop  * expr
  | ECall   of expr * expr list
  | ECtor   of string * expr list
  | ERecord of string * record_init_elem list
  | EField  of expr * string
  | EIf     of expr * expr * expr
  | ELet    of string * ty option * expr * expr
  | EMatch  of expr * (pat * expr) list
  | EArray  of expr * expr * expr         (* array(r, N, init) — allocate N slots in region r *)
  | EArrayLit of expr * expr list         (* array(r, [v0, v1, ...]) — allocate and initialize *)
  | ERegion of expr                       (* region(N) — heap arena, malloc'd block *)
  | EStackRegion of expr                  (* stack_region(N) — N literal, block on stack *)
  | EAlignedRegion of expr * expr         (* aligned_region(N, A) — heap, A-byte aligned *)
  | EIndex  of expr * expr                (* a[i] — read element *)
  | EAssignIdx of expr * expr * expr      (* a[i] := v — write element, returns int *)
  | ELen    of expr                       (* len(a) — array length *)

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
}

type record_decl = {
  rec_name        : string;
  rec_type_params : string list;
  rec_fields      : (string * ty) list;
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

type top_decl =
  | TopType   of type_decl
  | TopRecord of record_decl
  | TopFunc   of func
  | TopExtern of extern_decl

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
  | TyMeta { resolved = Some t; _ } -> show_ty t
  | TyMeta { id; resolved = None } -> Printf.sprintf "?%d" id

let show_pat = function
  | PWild              -> "_"
  | PCtor (c, [])      -> c
  | PCtor (c, vs)      ->
      Printf.sprintf "%s(%s)" c (String.concat ", " vs)

let show_binop = function
  | OpAdd -> "+"  | OpSub -> "-"
  | OpMul -> "*"  | OpDiv -> "/"  | OpMod -> "%"
  | OpEq  -> "==" | OpNeq -> "!="
  | OpLt  -> "<"  | OpGt  -> ">"
  | OpLe  -> "<=" | OpGe  -> ">="
  | OpAnd -> "&&" | OpOr  -> "||"

let show_unop = function
  | OpNeg -> "-"
  | OpNot -> "!"

let rec show_expr = function
  | EInt n          -> string_of_int n
  | EBool true      -> "true"
  | EBool false     -> "false"
  | EVar x          -> x
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
  | ELet (x, None, v, b)  ->
      Printf.sprintf "let %s = %s; %s" x (show_expr v) (show_expr b)
  | ELet (x, Some ty, v, b)  ->
      Printf.sprintf "let %s: %s = %s; %s"
        x (show_ty ty) (show_expr v) (show_expr b)
  | EMatch (e, arms) ->
      let arm_strs = List.map (fun (p, body) ->
        Printf.sprintf "%s => %s" (show_pat p) (show_expr body))
        arms
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

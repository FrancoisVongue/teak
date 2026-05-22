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
  | TyTuple of ty list           (* (T1, T2, ..., Tn) for n >= 2 — anonymous product *)
  | TyMeta of meta               (* unification variable, only inside the checker *)

(* The numeric type matrix: signed/unsigned integers 8..128 and floats,
   like Zig/Go. One registry, shared by parser (recognising a type name
   and a `to_<T>` cast), checker, and emit. `byte` aliases u8, `float`
   aliases f64; `int` is the word-size signed arithmetic type, handled
   separately as TyInt. *)
let numeric_c_type : (string * string) list =
  [ "i8", "int8_t"; "i16", "int16_t"; "i32", "int32_t"; "i64", "int64_t";
    "i128", "__int128";
    "u8", "uint8_t"; "u16", "uint16_t"; "u32", "uint32_t"; "u64", "uint64_t";
    "u128", "unsigned __int128";
    "f16", "_Float16"; "f32", "float"; "f64", "double"; "f128", "__float128";
    "byte", "uint8_t"; "float", "double" ]

let is_numeric_type (n : string) : bool = List.mem_assoc n numeric_c_type
let is_float_type (n : string) : bool =
  List.mem n ["f16"; "f32"; "f64"; "f128"; "float"]

type pat =
  | PBind of string                      (* lowercase ident — binds scrutinee to name; "_" = wildcard *)
  | PCtor of string * string list
  | POr   of pat list                    (* a | b | c — all must be PCtor or literals, no bindings *)
  | PInt  of int                         (* literal int pattern *)
  | PBool of bool                        (* literal bool pattern *)
  | PStr  of string                      (* literal Array[byte] pattern *)
  | PTuple of pat list                   (* (p1, p2, ..., pn) — destructure tuple scrutinee *)

type binop =
  | OpAdd | OpSub | OpMul | OpDiv | OpMod
  | OpEq  | OpNeq
  | OpLt  | OpGt  | OpLe  | OpGe
  | OpAnd | OpOr
  | OpBOr | OpBAnd | OpBXor             (* bitwise on int *)
  | OpShl | OpShr                       (* shifts on int *)

type unop = OpNeg | OpNot | OpBNot

type expr =
  | EInt    of int64
  | EFloat  of float
  | EBool   of bool
  | EVar    of string
  | EStringLit of string                  (* "..." — byte literal in static region *)
  | EBinop  of binop * expr * expr
  | EUnop   of unop  * expr
  | ECall   of expr * expr list
  | EFun    of (string * ty) list * ty * expr
                                          (* fn(params) -> ret { body } —
                                             anonymous function literal. Removed
                                             by the lambda-lifting pass (lift.ml)
                                             before the checker; only the lifter
                                             and resolver ever see it. *)
  | EClosure of expr * (string * ty) list * ty * expr
                                          (* closure(r, fn(params) -> ret { body })
                                             — a capturing lambda. The environment
                                             of captured locals is allocated in
                                             region r. The checker converts it to
                                             a lifted function + TEMakeClosure;
                                             the lifter leaves it intact. *)
  | ECtor   of string * expr list
  | ERecord of string * record_init_elem list
  | EField  of expr * string
  | EIf     of expr * expr * expr
  | ELet    of string * bool * ty option * expr * expr
                                          (* name, mut?, optional ascription, value, body *)
  | EAssign of string * expr              (* x := v — requires x to be mut *)
  | EAssignField of expr * string * expr  (* place.f := v — write a field of a
                                             place (var/field/index chain). *)
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
  | EToByte of expr                       (* to_byte(n) — truncate int to byte (u8) *)
  | EToU16  of expr                       (* to_u16(n)  — truncate int to u16 *)
  | EToU32  of expr                       (* to_u32(n)  — truncate int to u32 *)
  | EToU64  of expr                       (* to_u64(n)  — int to u64 (signed reinterpret) *)
  | EToFloat of expr                      (* to_float(n) — int → float *)
  | ECast   of string * expr              (* to_<T>(e) — convert e to numeric type T *)
  | ECAlloc of ty * expr                  (* c_alloc[T](n) — malloc n*sizeof(T), returns *T *)
  | ECFree  of expr                       (* c_free(p) — free raw pointer *)
  | ENullPtr of ty                        (* null_ptr[T]() — typed NULL *)
  | EIsNull of expr                       (* is_null(p) — NULL check *)
  | EArrayData of expr                    (* array_data(a) — *T view of Array[T] bytes *)
  | EPtrCast of ty * expr                 (* ptr_cast[T](e) — reinterpret raw pointer/address as *T *)
  | ETryAt  of expr * expr                (* try_at(a, i) — None on dangling/oob *)
  | EDrop   of expr                       (* drop(x) — consume linear value, run its drop fn *)
  | EDeref  of expr                       (* *p — pointer deref *)
  (* Stage 3 — completion-based concurrency. See STAGE3_ASYNC.md. *)
  | EAwait    of expr                     (* await op — suspend until op completes *)
  | EAwaitAll of expr list                (* await all { e1, e2, ... } — static concurrent block *)
  | EAwaitAllDyn of expr                  (* await all <iterable> — dynamic concurrent join *)
  | ESpawn  of expr                       (* spawn f(args) — detached or joinable task *)
  (* `yield` is parser sugar for `EAwait (ECall (EVar "orto_nop", []))` —
     there is no EYield AST node. *)
  | EForStream of string * expr * expr    (* for x in <stream> { body } — multishot loop *)
  | ETuple    of expr list                (* (e1, e2, ..., en) for n >= 2 *)
  | ETupleIdx of expr * int               (* t.0, t.1 — bounds-checked at type-check time *)
  | ELetTuple of string list * expr * expr
                                          (* let (x, y, z) = expr; body — destructuring let *)
  | EArena    of string * expr * expr     (* arena r = <region-expr>; body
                                             Scope-bound region binding. Value
                                             must type to Region. Lowers to a
                                             linear let in the checker; mono and
                                             emit never see EArena. *)
  | EPrint    of bool * expr              (* print/println intrinsic.
                                             bool = newline?  Inner expr is
                                             either a tuple literal (each
                                             component printed in order) or
                                             a single printable scalar.
                                             Lowers to one writev syscall. *)

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
  (* The return type drives the calling convention. Write it
     directly: `Task[T]` for SQE-prep externs lowered under `await`,
     `Stream[T]` for multishot sources drained by `for x in call`,
     bare `T` for ordinary sync FFI. The compiler picks the C-side
     signature shape from the declared return type. *)
  ext_return_ty : ty;
}

(* `use a::b::c::{x, y};` — selective import from a namespace.
   The path may be any depth; in the common single-component case
   `use foo::{x}` the list has one element ["foo"]. Items must be a
   non-empty list; bare `use mod;` is not supported. With the `pub`
   prefix (`pub use a::b::{x};`), the imported names are re-exported:
   clients of THIS namespace can `use this::{x}`. *)
type use_decl = {
  use_module : string list;
  use_items  : string list;
  use_pub    : bool;
}

(* `type Bytes = Array[byte];` — a plain alias. Resolved away by the
   resolver before type checking; no runtime presence. Non-generic only. *)
type alias_decl = {
  alias_name : string;
  alias_ty   : ty;
}

(* `const NAME: T = expr;` — a named compile-time value. Resolved away by
   the resolver into a 0-argument function `fn NAME() -> T { expr }`, and
   every use of NAME is rewritten to a call NAME(). The value lives at the
   top level (no region/locals in scope), so it can only be a literal,
   arithmetic, or another const — never an allocation. *)
type const_decl = {
  const_name  : string;
  const_ty    : ty;
  const_value : expr;
}

(* `test "human description" { body }` — top-level test block.
   In normal compile mode (no --test) test blocks are silently
   ignored. In --test mode the driver generates a `main` that
   runs every test and reports PASS/FAIL with the human name. *)
type test_decl = {
  test_name : string;   (* the literal from `test "..." { … }` *)
  test_body : expr;     (* must return int — 0 = pass, !=0 = fail *)
}

type top_decl =
  | TopType   of type_decl
  | TopRecord of record_decl
  | TopFunc   of func
  | TopExtern of extern_decl
  | TopUse    of use_decl
  | TopAlias  of alias_decl
  | TopConst  of const_decl
  | TopTest   of test_decl
  | TopNamespace of string list * top_decl list
                                          (* `namespace a::b::c { ... }` —
                                             decls inside live in that
                                             dotted namespace path *)

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
  | TyTuple ts ->
      Printf.sprintf "(%s)" (String.concat ", " (List.map show_ty ts))
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
  | PTuple ps ->
      "(" ^ String.concat ", " (List.map show_pat ps) ^ ")"

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
  | EInt n          -> Int64.to_string n
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
  | EFun (params, ret, body) ->
      Printf.sprintf "fn(%s) -> %s { %s }"
        (String.concat ", "
           (List.map (fun (n, t) ->
              Printf.sprintf "%s: %s" n (show_ty t)) params))
        (show_ty ret) (show_expr body)
  | EClosure (r, params, ret, body) ->
      Printf.sprintf "closure(%s, fn(%s) -> %s { %s })"
        (show_expr r)
        (String.concat ", "
           (List.map (fun (n, t) ->
              Printf.sprintf "%s: %s" n (show_ty t)) params))
        (show_ty ret) (show_expr body)
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
  | EAssignField (p, f, v) ->
      Printf.sprintf "(%s.%s := %s)" (show_expr p) f (show_expr v)
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
  | EToU16 e -> Printf.sprintf "to_u16(%s)" (show_expr e)
  | EToU32 e -> Printf.sprintf "to_u32(%s)" (show_expr e)
  | EToU64 e -> Printf.sprintf "to_u64(%s)" (show_expr e)
  | EToFloat e -> Printf.sprintf "to_float(%s)" (show_expr e)
  | ECast (t, e) -> Printf.sprintf "to_%s(%s)" t (show_expr e)
  | ECAlloc (t, n) ->
      Printf.sprintf "c_alloc[%s](%s)" (show_ty t) (show_expr n)
  | ECFree p -> Printf.sprintf "c_free(%s)" (show_expr p)
  | ENullPtr t -> Printf.sprintf "null_ptr[%s]()" (show_ty t)
  | EIsNull p -> Printf.sprintf "is_null(%s)" (show_expr p)
  | EArrayData a -> Printf.sprintf "array_data(%s)" (show_expr a)
  | EPtrCast (t, e) -> Printf.sprintf "ptr_cast[%s](%s)" (show_ty t) (show_expr e)
  | ETryAt (a, i) -> Printf.sprintf "try_at(%s, %s)" (show_expr a) (show_expr i)
  | EDrop e -> Printf.sprintf "drop(%s)" (show_expr e)
  | EDeref p -> Printf.sprintf "*%s" (show_expr p)
  | EAwait e -> Printf.sprintf "await %s" (show_expr e)
  | EAwaitAll branches ->
      Printf.sprintf "await all { %s }"
        (String.concat ", " (List.map show_expr branches))
  | EAwaitAllDyn e -> Printf.sprintf "await all %s" (show_expr e)
  | ESpawn e -> Printf.sprintf "spawn %s" (show_expr e)
  | EForStream (x, s, b) ->
      Printf.sprintf "for %s in %s { %s }" x (show_expr s) (show_expr b)
  | ETuple es ->
      Printf.sprintf "(%s)" (String.concat ", " (List.map show_expr es))
  | ETupleIdx (e, i) ->
      Printf.sprintf "%s.%d" (show_expr e) i
  | ELetTuple (vs, v, b) ->
      Printf.sprintf "let (%s) = %s; %s"
        (String.concat ", " vs) (show_expr v) (show_expr b)
  | EArena (x, v, b) ->
      Printf.sprintf "arena %s = %s; %s" x (show_expr v) (show_expr b)
  | EPrint (nl, e) ->
      Printf.sprintf "%s(%s)" (if nl then "println" else "print") (show_expr e)

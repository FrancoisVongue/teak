(* Type checker with Hindley-Milner inference.

   Stage A + function types:
     - TyFun for `fn(args) -> ret` types.
     - Indirect calls: EApply on a function-typed value.
     - Function references: an EVar bound to a global fn becomes a
       TEFnRef in the typed AST (a first-class function value).

   No let-generalization. Builtins cannot be used as values
   (call them directly with `add(a, b)` or via operator `a + b`). *)

open Ast

exception Type_error of string

(* ---------- typed AST ---------- *)

module T = struct
  type expr =
    | TEInt    of int
    | TEBool   of bool
    | TEVar    of string * ty
    | TEStringLit of string
                  (* "..." — byte literal, lives in the static region *)
    | TEFnRef  of string * ty list * ty
                  (* fn as a value: name, type args, fn type *)
    | TECall   of expr * expr list * ty
                  (* apply callee expr to args; result type *)
    | TEBinop  of binop * expr * expr * ty
    | TEUnop   of unop  * expr * ty
    | TECtor   of string * ty list * expr list * ty
    | TERecord of string * ty list * (string * expr) list * ty
                  (* record name, type args, field assignments, result *)
    | TEField  of expr * string * ty
                  (* record expr, field name, field type *)
    | TEIf     of expr * expr * expr * ty
    | TELet    of string * ty * expr * expr * ty * bool
                  (* name, var_ty, value, body, body_ty, auto_drop.
                     auto_drop=true means: when control leaves this Let,
                     emit a runtime drop of the bound variable. Used for
                     Region bindings that are not consumed. *)
    | TEMatch  of expr * ty * (pat * expr) list * ty
    | TEArray  of expr * expr * expr * ty
                  (* array(r, N, init) — allocate in region r, result is Array[T] *)
    | TEArrayLit of expr * expr list * ty
                  (* array(r, [v0..vN]) — allocate in r, init each slot *)
    | TERegion of expr * ty
                  (* region(N) — heap arena *)
    | TEStackRegion of expr * ty
                  (* stack_region(N) — block on stack; N is int literal *)
    | TEAlignedRegion of expr * expr * ty
                  (* aligned_region(N, A) — heap arena, A-byte aligned *)
    | TEIndex  of expr * expr * ty
                  (* a[i] — result is T (element type) *)
    | TEAssignIdx of expr * expr * expr * ty
                  (* a[i] := v — result is int (placeholder for unit) *)
    | TELen    of expr * ty
                  (* len(a) — result is int *)
    | TESlice  of expr * expr * expr * ty
                  (* slice(a, lo, hi) — sub-handle into the same region *)
    | TEToInt  of expr
                  (* to_int(b) — widen byte to int *)
    | TEToByte of expr
                  (* to_byte(n) — truncate int to byte *)
    | TECAlloc of ty * expr * ty
                  (* c_alloc[T](n) — elem type T, count, result type TyPtr T *)
    | TECFree  of expr
                  (* c_free(p) *)
    | TENullPtr of ty
                  (* null_ptr[T]() — result type TyPtr T *)
    | TEIsNull of expr
                  (* is_null(p) -> bool *)
    | TEArrayData of expr * ty
                  (* array_data(a: Array[T]) -> TyPtr T *)
    | TETryAt of expr * expr * ty
                  (* try_at(a, i) — third field is result Option[T] *)
    | TEDrop  of expr * ty
                  (* drop(x) — second field is x's (linear) type *)
    | TEDeref  of expr * ty
                  (* p deref — second field is element type T *)
    | TEAssign of string * expr * ty
                  (* x := v — third field is the type of x *)
    | TEWhile  of expr * expr
                  (* while cond { body } — always int 0 *)
    | TEBreak
    | TEContinue
    | TEReturn of expr * ty
                  (* return v — second field is the enclosing fn's return ty *)

  type func = {
    name        : string;
    type_params : string list;
    params      : (string * ty) list;
    return_ty   : ty;
    body        : expr;
  }

  type extern = {
    name      : string;
    params    : (string * ty) list;
    return_ty : ty;
  }

  type program = {
    types   : type_decl list;
    records : record_decl list;
    funcs   : func list;
    externs : extern list;
  }
end

(* ---------- operator typing ----------

   Operators are AST primitives, not callable functions. No name is
   exposed for them — `a + b` is the only way to add. *)

type op_typing =
  | OpFixed of ty * ty     (* operand type, result type *)
  | OpEqual                (* both operands same type; type must be int or bool *)

let binop_typing = function
  | OpAdd | OpSub | OpMul | OpDiv | OpMod -> OpFixed (TyInt, TyInt)
  | OpLt | OpGt | OpLe | OpGe              -> OpFixed (TyInt, TyBool)
  | OpAnd | OpOr                            -> OpFixed (TyBool, TyBool)
  | OpEq | OpNeq                            -> OpEqual

let unop_typing = function
  | OpNeg -> (TyInt, TyInt)
  | OpNot -> (TyBool, TyBool)

(* ---------- C reserved words ----------

   Any user-supplied identifier that would end up as a C identifier
   (function/type/ctor name; param/let/pattern var) must not clash
   with a C keyword. Otherwise the generated source won't compile. *)

let c_reserved = [
  "auto"; "break"; "case"; "char"; "const"; "continue"; "default";
  "do"; "double"; "else"; "enum"; "extern"; "float"; "for"; "goto";
  "if"; "inline"; "int"; "long"; "register"; "restrict"; "return";
  "short"; "signed"; "sizeof"; "static"; "struct"; "switch";
  "typedef"; "union"; "unsigned"; "void"; "volatile"; "while";
  "_Bool"; "_Complex"; "_Imaginary"; "_Atomic"; "_Generic";
  "_Noreturn"; "_Static_assert"; "_Thread_local"; "_Alignas";
  "_Alignof";
]

let check_not_c_reserved (kind : string) (name : string) : unit =
  if List.mem name c_reserved then
    raise (Type_error
      (Printf.sprintf "%s name %S is a reserved C keyword — rename it"
         kind name))

(* ---------- meta variables, zonk, unify ---------- *)

let meta_counter = ref 0
let drop_name_counter = ref 0

(* Depth of the current while loop nest. break/continue require > 0.
   Reset at each function-body entry. *)
let loop_depth = ref 0

(* The return type of the function currently being checked, so EReturn
   can verify the type of its expression. Reset on every check_func. *)
let current_return_ty : ty option ref = ref None

(* Set of type names that are linear: Region (always) plus every
   user-declared `linear struct/enum`. Populated by build_env. *)
let linear_type_names : (string, unit) Hashtbl.t = Hashtbl.create 8

let reset_linear_table () =
  Hashtbl.clear linear_type_names;
  Hashtbl.add linear_type_names "Region" ()

let mark_linear n = Hashtbl.replace linear_type_names n ()

let is_linear_name n = Hashtbl.mem linear_type_names n

(* The mangled name of the drop function for a linear type. Given the
   type's (possibly mangled) name "<mod>__<base>", returns
   "<mod>__drop_<base>". For unmangled names like "Region" (builtin),
   returns "drop_Region". *)
let drop_fn_name_for (type_name : string) : string =
  let n = String.length type_name in
  let rec find_dd i =
    if i + 1 >= n then None
    else if type_name.[i] = '_' && type_name.[i + 1] = '_' then Some i
    else find_dd (i + 1)
  in
  match find_dd 0 with
  | Some i ->
      let prefix = String.sub type_name 0 (i + 2) in
      let base   = String.sub type_name (i + 2) (n - i - 2) in
      prefix ^ "drop_" ^ base
  | None -> "drop_" ^ type_name
let fresh_meta () : meta =
  incr meta_counter;
  { id = !meta_counter; resolved = None }

let rec prune (t : ty) : ty =
  match t with
  | TyMeta ({ resolved = Some inner; _ } as m) ->
      let r = prune inner in
      m.resolved <- Some r;
      r
  | _ -> t

let is_linear_ty (t : ty) : bool =
  match prune t with
  | TyApp (n, _) -> is_linear_name n
  | _ -> false

let rec zonk (t : ty) : ty =
  match prune t with
  | TyInt -> TyInt
  | TyBool -> TyBool
  | TyVar n -> TyVar n
  | TyApp (n, args) -> TyApp (n, List.map zonk args)
  | TyFun (args, ret) -> TyFun (List.map zonk args, zonk ret)
  | TyPtr inner -> TyPtr (zonk inner)
  | TyMeta _ as t -> t

let rec occurs (m : meta) (t : ty) : bool =
  match prune t with
  | TyInt | TyBool | TyVar _ -> false
  | TyApp (_, args) -> List.exists (occurs m) args
  | TyFun (args, ret) ->
      List.exists (occurs m) args || occurs m ret
  | TyPtr inner -> occurs m inner
  | TyMeta m' -> m.id = m'.id

let rec unify (t1 : ty) (t2 : ty) : unit =
  let t1 = prune t1 and t2 = prune t2 in
  match t1, t2 with
  | TyInt, TyInt -> ()
  | TyBool, TyBool -> ()
  | TyVar a, TyVar b when a = b -> ()
  | TyApp (n1, a1), TyApp (n2, a2)
    when n1 = n2 && List.length a1 = List.length a2 ->
      List.iter2 unify a1 a2
  | TyFun (a1, r1), TyFun (a2, r2)
    when List.length a1 = List.length a2 ->
      List.iter2 unify a1 a2;
      unify r1 r2
  | TyPtr a, TyPtr b -> unify a b
  | TyMeta m1, TyMeta m2 when m1.id = m2.id -> ()
  | TyMeta m, t | t, TyMeta m ->
      if occurs m t then
        raise (Type_error
          (Printf.sprintf
             "occurs check failed (would build an infinite type): ?%d in %s"
             m.id (show_ty t)));
      m.resolved <- Some t
  | _ ->
      raise (Type_error
        (Printf.sprintf "type mismatch: %s vs %s"
           (show_ty t1) (show_ty t2)))

(* ---------- substitution and instantiation ---------- *)

let rec subst_ty (subst : (string * ty) list) (t : ty) : ty =
  match t with
  | TyInt | TyBool -> t
  | TyVar n ->
      (try List.assoc n subst with Not_found -> t)
  | TyApp (n, args) -> TyApp (n, List.map (subst_ty subst) args)
  | TyFun (args, ret) ->
      TyFun (List.map (subst_ty subst) args, subst_ty subst ret)
  | TyPtr inner -> TyPtr (subst_ty subst inner)
  | TyMeta _ -> t

let make_instantiation (tparams : string list) : (string * ty) list * ty list =
  let metas = List.map (fun _ -> TyMeta (fresh_meta ())) tparams in
  let subst = List.combine tparams metas in
  (subst, metas)

(* ---------- environments ---------- *)

type ctor_info = {
  ctor_owner       : string;
  ctor_owner_params: string list;
  ctor_args        : ty list;
  ctor_index       : int;
}

type env = {
  types   : (string * type_decl) list;
  records : (string * record_decl) list;
  ctors   : (string * ctor_info) list;
  fns     : (string * (string list * (ty list * ty))) list;
}

let split_program (prog : program)
  : type_decl list * record_decl list * func list * extern_decl list =
  let rec loop ts rs fs es = function
    | []                  -> (List.rev ts, List.rev rs, List.rev fs, List.rev es)
    | TopType t   :: rest -> loop (t :: ts) rs fs es rest
    | TopRecord r :: rest -> loop ts (r :: rs) fs es rest
    | TopFunc f   :: rest -> loop ts rs (f :: fs) es rest
    | TopExtern e :: rest -> loop ts rs fs (e :: es) rest
    | TopUse _    :: _    ->
        failwith "check: TopUse left in program — \
                  the resolver should have eliminated all `use` decls"
    | TopAlias _  :: _    ->
        failwith "check: TopAlias left in program — \
                  the resolver should have inlined all `type` aliases"
  in
  loop [] [] [] [] prog

(* ---------- type validation ---------- *)

(* Tests whether a type contains a linear builtin or user-linear type
   anywhere except behind a function arrow or raw pointer. Linear types
   may only live as the top-level type of a name — never as a record
   field, variant argument, or type-argument of a non-linear container. *)
let rec ty_contains_linear (t : ty) : bool =
  match t with
  | TyApp (n, _) when is_linear_name n -> true
  | TyApp (_, args) -> List.exists ty_contains_linear args
  | TyFun _ -> false
  | TyPtr _ -> false
  | TyInt | TyBool | TyVar _ | TyMeta _ -> false

(* When a generic is instantiated (function call, ctor application,
   record literal), every type meta receives values by copy. Resolving
   it to a linear type (Region) violates that contract — Region cannot
   be copied, only moved. Catch this at the call site rather than
   crashing later in mono. *)
let check_instantiation (where : string) (metas : ty list) : unit =
  List.iter (fun m ->
    let mz = zonk m in
    if ty_contains_linear mz then
      raise (Type_error
        (Printf.sprintf
           "%s: cannot instantiate a generic type parameter with the \
            linear type %s — only copyable types are allowed here"
           where (show_ty mz))))
    metas

let rec validate_ty
  (type_env : (string * type_decl) list)
  (record_env : (string * record_decl) list)
  (in_scope : string list)
  (t : ty) : ty =
  match t with
  | TyInt | TyBool -> t
  | TyVar _ -> t
  | TyMeta _ -> t
  | TyApp (n, args) ->
      let args = List.map (validate_ty type_env record_env in_scope) args in
      (* A linear type cannot appear in data position — only as the
         immediate top-level type of a name (variable, parameter,
         function return). We enforce this by forbidding linear types
         in any type argument of any TyApp. The check on field types
         and variant arg types happens separately after build_env.
         Function types are not "data position" — they hide their
         contents, so fn(...) -> Region stays legal. *)
      List.iter (fun arg ->
        if ty_contains_linear arg then
          raise (Type_error
            (Printf.sprintf
               "A linear type is not allowed as a type argument of %S — \
                linear types must be a top-level type of a name, not nested in data"
               n))) args;
      if List.mem n in_scope then begin
        if args <> [] then
          raise (Type_error
            (Printf.sprintf
               "type parameter %S cannot take type arguments" n));
        TyVar n
      end else if n = "Array" then begin
        (* Array[T] is a built-in unary type constructor — handle to a
           region-allocated buffer. The wrapper is copyable. *)
        if List.length args <> 1 then
          raise (Type_error
            (Printf.sprintf
               "Array expects exactly 1 type argument, got %d"
               (List.length args)));
        TyApp ("Array", args)
      end else if n = "Region" then begin
        (* Region is a built-in nullary type — owned arena. Linear. *)
        if List.length args <> 0 then
          raise (Type_error
            (Printf.sprintf
               "Region takes no type arguments, got %d"
               (List.length args)));
        TyApp ("Region", [])
      end else if n = "byte" then begin
        (* byte is a built-in nullary primitive — 1 byte, unsigned. *)
        if List.length args <> 0 then
          raise (Type_error
            (Printf.sprintf
               "byte takes no type arguments, got %d"
               (List.length args)));
        TyApp ("byte", [])
      end else
        (match List.assoc_opt n type_env with
         | Some td ->
             let expected = List.length td.type_params in
             let got = List.length args in
             if expected <> got then
               raise (Type_error
                 (Printf.sprintf
                    "type %S expects %d type argument(s), got %d"
                    n expected got));
             TyApp (n, args)
         | None ->
             (match List.assoc_opt n record_env with
              | Some rd ->
                  let expected = List.length rd.rec_type_params in
                  let got = List.length args in
                  if expected <> got then
                    raise (Type_error
                      (Printf.sprintf
                         "type %S expects %d type argument(s), got %d"
                         n expected got));
                  TyApp (n, args)
              | None ->
                  raise (Type_error
                    (Printf.sprintf "unknown type %S" n))))
  | TyFun (args, ret) ->
      TyFun (List.map (validate_ty type_env record_env in_scope) args,
             validate_ty type_env record_env in_scope ret)
  | TyPtr inner ->
      TyPtr (validate_ty type_env record_env in_scope inner)

(* ---------- building environment ---------- *)

let build_env
  (types : type_decl list)
  (records : record_decl list)
  (funcs : func list)
  (externs : extern_decl list) : env =
  reset_linear_table ();
  List.iter (fun (td : type_decl) ->
    if td.is_linear then mark_linear td.type_name) types;
  List.iter (fun (rd : record_decl) ->
    if rd.rec_is_linear then mark_linear rd.rec_name) records;
  let seen_types = Hashtbl.create 16 in
  List.iter (fun (td : type_decl) ->
    check_not_c_reserved "type" td.type_name;
    if Hashtbl.mem seen_types td.type_name then
      raise (Type_error
        (Printf.sprintf "type %S declared twice" td.type_name));
    Hashtbl.add seen_types td.type_name ();
    let seen_params = Hashtbl.create 4 in
    List.iter (fun p ->
      if Hashtbl.mem seen_params p then
        raise (Type_error
          (Printf.sprintf
             "duplicate type parameter %S in type %S" p td.type_name));
      Hashtbl.add seen_params p ()) td.type_params) types;
  List.iter (fun (rd : record_decl) ->
    check_not_c_reserved "type" rd.rec_name;
    if Hashtbl.mem seen_types rd.rec_name then
      raise (Type_error
        (Printf.sprintf "type %S declared twice" rd.rec_name));
    Hashtbl.add seen_types rd.rec_name ();
    let seen_params = Hashtbl.create 4 in
    List.iter (fun p ->
      if Hashtbl.mem seen_params p then
        raise (Type_error
          (Printf.sprintf
             "duplicate type parameter %S in type %S" p rd.rec_name));
      Hashtbl.add seen_params p ()) rd.rec_type_params;
    let seen_fields = Hashtbl.create 4 in
    List.iter (fun (fname, _) ->
      check_not_c_reserved "field" fname;
      if Hashtbl.mem seen_fields fname then
        raise (Type_error
          (Printf.sprintf
             "duplicate field %S in record %S" fname rd.rec_name));
      Hashtbl.add seen_fields fname ()) rd.rec_fields) records;

  let type_env = List.map (fun (td : type_decl) ->
    (td.type_name, td)) types in
  let record_env = List.map (fun (rd : record_decl) ->
    (rd.rec_name, rd)) records in

  let types =
    List.map (fun (td : type_decl) ->
      let in_scope = td.type_params in
      let variants =
        List.map (fun v ->
          let arg_tys =
            List.map (validate_ty type_env record_env in_scope) v.arg_tys
          in
          List.iter (fun aty ->
            if ty_contains_linear aty then
              raise (Type_error
                (Printf.sprintf
                   "constructor %S of %S: a linear type (Own/Array) cannot \
                    be a variant argument — linear types must be top-level \
                    types of a name"
                   v.ctor_name td.type_name))) arg_tys;
          { v with arg_tys })
          td.variants
      in
      { td with variants }) types
  in
  let records =
    List.map (fun (rd : record_decl) ->
      let in_scope = rd.rec_type_params in
      let fields =
        List.map (fun (fname, fty) ->
          let fty = validate_ty type_env record_env in_scope fty in
          if ty_contains_linear fty then
            raise (Type_error
              (Printf.sprintf
                 "field %S of record %S: a linear type (Own/Array) cannot \
                  be a record field — linear types must be top-level types \
                  of a name"
                 fname rd.rec_name));
          (fname, fty))
          rd.rec_fields
      in
      { rd with rec_fields = fields }) records
  in
  let type_env = List.map (fun (td : type_decl) ->
    (td.type_name, td)) types in
  let record_env = List.map (fun (rd : record_decl) ->
    (rd.rec_name, rd)) records in

  let seen_ctors = Hashtbl.create 32 in
  let ctor_env =
    List.concat_map (fun (td : type_decl) ->
      List.mapi (fun i v ->
        check_not_c_reserved "constructor" v.ctor_name;
        if Hashtbl.mem seen_ctors v.ctor_name then
          raise (Type_error
            (Printf.sprintf "constructor %S declared twice" v.ctor_name));
        Hashtbl.add seen_ctors v.ctor_name ();
        (v.ctor_name,
         { ctor_owner = td.type_name;
           ctor_owner_params = td.type_params;
           ctor_args = v.arg_tys;
           ctor_index = i })
      ) td.variants) types
  in

  let seen_fns = Hashtbl.create 32 in
  let user_sigs =
    List.map (fun f ->
      check_not_c_reserved "function" f.name;
      if Hashtbl.mem seen_fns f.name then
        raise (Type_error
          (Printf.sprintf "function %S defined twice" f.name));
      Hashtbl.add seen_fns f.name ();
      List.iter (fun (pname, _) ->
        check_not_c_reserved "parameter" pname) f.params;
      let seen_params = Hashtbl.create 4 in
      List.iter (fun p ->
        if Hashtbl.mem seen_params p then
          raise (Type_error
            (Printf.sprintf
               "duplicate type parameter %S in function %S" p f.name));
        Hashtbl.add seen_params p ()) f.type_params;
      let in_scope = f.type_params in
      let param_tys =
        List.map (fun (_, t) ->
          validate_ty type_env record_env in_scope t) f.params
      in
      let ret_ty =
        validate_ty type_env record_env in_scope f.return_ty in
      (match prune ret_ty with
       | TyApp ("Region", _) ->
           raise (Type_error
             (Printf.sprintf
                "function %S cannot return Region — \
                 each Region must be created in the scope that frees it; \
                 caller should call region(...) and pass it in"
                f.name))
       | _ -> ());
      (f.name, (f.type_params, (param_tys, ret_ty)))) funcs
  in
  let extern_sigs =
    List.map (fun (e : extern_decl) ->
      check_not_c_reserved "extern function" e.ext_name;
      if Hashtbl.mem seen_fns e.ext_name then
        raise (Type_error
          (Printf.sprintf
             "extern %S clashes with another function or extern" e.ext_name));
      Hashtbl.add seen_fns e.ext_name ();
      List.iter (fun (pname, _) ->
        check_not_c_reserved "parameter" pname) e.ext_params;
      let param_tys =
        List.map (fun (_, t) ->
          validate_ty type_env record_env [] t) e.ext_params
      in
      let ret_ty = validate_ty type_env record_env [] e.ext_return_ty in
      (e.ext_name, ([], (param_tys, ret_ty)))) externs
  in
  let env = { types = type_env;
              records = record_env;
              ctors = ctor_env;
              fns = user_sigs @ extern_sigs } in
  (* For every user-declared linear type, require a matching drop fn
     in the same module. The fn is found by name convention. *)
  let check_drop_fn type_name =
    let fn_name = drop_fn_name_for type_name in
    if not (List.mem_assoc fn_name env.fns) then
      raise (Type_error
        (Printf.sprintf
           "linear type %S requires a drop function %S in the same module \
            (signature: fn %s(<param>: %s) -> int)"
           type_name fn_name fn_name type_name))
  in
  List.iter (fun (td : type_decl) ->
    if td.is_linear then check_drop_fn td.type_name) types;
  List.iter (fun (rd : record_decl) ->
    if rd.rec_is_linear then check_drop_fn rd.rec_name) records;
  env

(* ---------- recursive type detection ---------- *)

let check_no_recursive_types
  (type_env : (string * type_decl) list)
  (record_env : (string * record_decl) list) : unit =
  let is_known n =
    List.mem_assoc n type_env || List.mem_assoc n record_env
  in
  let rec deps_in_ty acc = function
    | TyInt | TyBool | TyVar _ | TyMeta _ -> acc
    | TyApp ("Ref", _) | TyApp ("Own", _) ->
        (* Ref and Own both break by-value cycles — they're pointer-sized
           regardless of what's inside. *)
        acc
    | TyApp (n, args) ->
        let acc = if is_known n then n :: acc else acc in
        List.fold_left deps_in_ty acc args
    | TyFun (args, ret) ->
        let acc = List.fold_left deps_in_ty acc args in
        deps_in_ty acc ret
    | TyPtr _ -> acc   (* pointers break by-value cycles *)
  in
  let direct_deps name : string list =
    match List.assoc_opt name type_env with
    | Some td ->
        List.fold_left
          (fun acc v -> List.fold_left deps_in_ty acc v.arg_tys)
          [] td.variants
    | None ->
        let rd = List.assoc name record_env in
        List.fold_left
          (fun acc (_, fty) -> deps_in_ty acc fty)
          [] rd.rec_fields
  in
  let rec visit (root : string) (current : string) (path : string list) : unit =
    let deps = direct_deps current in
    List.iter (fun dep ->
      if dep = root then
        raise (Type_error
          (Printf.sprintf
             "recursive type %S (cycle: %s) — use Own[...] or Ref[...] for indirection"
             root
             (String.concat " -> " (List.rev (dep :: path)))))
      else if not (List.mem dep path) then
        visit root dep (dep :: path)) deps
  in
  List.iter (fun (n, _) -> visit n n [n]) type_env;
  List.iter (fun (n, _) -> visit n n [n]) record_env

(* ---------- pattern checking ----------

   The scrutinee's type decides what kinds of patterns are allowed,
   and what exhaustivity means. Centralised so each branch is short
   and follows from a single rule. *)

type scrut_kind =
  | SK_Adt   of string             (* ADT — match by ctor *)
  | SK_Int                         (* int — literal patterns, default required *)
  | SK_Byte                        (* byte — same rules as int *)
  | SK_Bool                        (* bool — true/false, exhaustive if both covered *)
  | SK_Bytes                       (* Array[byte] — string literal patterns *)

let scrutinee_kind (env : env) (t : ty) : scrut_kind =
  match prune t with
  | TyInt  -> SK_Int
  | TyBool -> SK_Bool
  | TyApp ("byte", []) -> SK_Byte
  | TyApp ("Array", [inner]) ->
      (match prune inner with
       | TyApp ("byte", []) -> SK_Bytes
       | _ ->
           raise (Type_error
             (Printf.sprintf
                "match scrutinee must be int, bool, byte, Array[byte], or an ADT, got %s"
                (show_ty (zonk t)))))
  | TyApp (n, _) when List.mem_assoc n env.types -> SK_Adt n
  | _ ->
      raise (Type_error
        (Printf.sprintf
           "match scrutinee must be int, bool, byte, Array[byte], or an ADT, got %s"
           (show_ty (zonk t))))

let is_catchall_pat = function
  | PBind _ -> true
  | _ -> false

(* Reject patterns that don't belong on this scrutinee kind. Also
   reject malformed or-patterns. Doesn't enforce exhaustivity — that's
   the next step. *)
let rec pat_compatible_with_kind kind p =
  match kind, p with
  | _, PBind _ -> true
  | SK_Adt _, PCtor _ -> true
  | SK_Int, PInt _ | SK_Byte, PInt _ -> true
  | SK_Bool, PBool _ -> true
  | SK_Bytes, PStr _ -> true
  | _, POr pats -> List.for_all (pat_compatible_with_kind kind) pats
  | _, _ -> false

let pat_kind_name = function
  | SK_Adt n -> Printf.sprintf "ADT %s" n
  | SK_Int -> "int"
  | SK_Byte -> "byte"
  | SK_Bool -> "bool"
  | SK_Bytes -> "Array[byte]"

(* Walk arms left-to-right enforcing:
     - patterns suit the scrutinee kind
     - or-pattern sub-arms suit the kind too, and forbid bindings
     - ADT-specific: ctor exists in the ADT
     - no arm after a catch-all *)
let check_match_arms_structure
  (env : env)
  (kind : scrut_kind)
  (arms : (pat * 'a) list) : unit =
  let all_ctors = match kind with
    | SK_Adt name ->
        let td = List.assoc name env.types in
        List.map (fun v -> v.ctor_name) td.variants
    | _ -> []
  in
  let seen_ctors = Hashtbl.create 8 in
  let catchall_seen = ref false in
  let register_ctor c =
    if Hashtbl.mem seen_ctors c then
      raise (Type_error
        (Printf.sprintf "duplicate pattern %S in match" c));
    if not (List.mem c all_ctors) then
      raise (Type_error
        (Printf.sprintf
           "constructor %S does not belong to %s"
           c (pat_kind_name kind)));
    Hashtbl.add seen_ctors c ()
  in
  List.iter (fun (p, _) ->
    if !catchall_seen then
      raise (Type_error "unreachable pattern after wildcard or bind");
    if not (pat_compatible_with_kind kind p) then
      raise (Type_error
        (Printf.sprintf
           "pattern %s is not valid for a %s scrutinee"
           (show_pat p) (pat_kind_name kind)));
    if is_catchall_pat p then catchall_seen := true
    else match p with
    | PCtor (c, _) -> register_ctor c
    | POr pats ->
        List.iter (function
          | PCtor (c, vs) ->
              if vs <> [] then
                raise (Type_error
                  (Printf.sprintf
                     "or-pattern arm %S(...) must not bind variables"
                     c));
              register_ctor c
          | PInt _ | PBool _ | PStr _ -> ()
          | _ ->
              raise (Type_error
                "or-pattern arms must be constructors or literals, with no bindings"))
          pats
    | PInt _ | PBool _ | PStr _ -> ()
    | _ -> ()
  ) arms;
  (* Exhaustivity. Only ADT and Bool can be exhaustive without a
     catch-all — Int / Byte / Bytes always need one. *)
  if not !catchall_seen then begin
    match kind with
    | SK_Adt _ ->
        let missing =
          List.filter (fun c -> not (Hashtbl.mem seen_ctors c)) all_ctors
        in
        if missing <> [] then
          raise (Type_error
            (Printf.sprintf "non-exhaustive match: missing %s"
               (String.concat ", " missing)))
    | SK_Bool ->
        let bool_present b =
          List.exists (fun (p, _) ->
            let rec check = function
              | PBool b' -> b' = b
              | POr ps -> List.exists check ps
              | _ -> false
            in check p) arms
        in
        if not (bool_present true && bool_present false) then
          raise (Type_error
            "non-exhaustive bool match: must cover both `true` and `false`, \
             or include a wildcard arm")
    | _ ->
        raise (Type_error
          (Printf.sprintf
             "non-exhaustive %s match: a wildcard (`_`) or bind arm is required \
              because the value domain is not enumerable"
             (pat_kind_name kind)))
  end

(* vars carries (name, (type, is_mut)) so EAssign can verify mutability. *)
let rec infer (env : env) (tparams : string list)
  (vars : (string * (ty * bool)) list) (e : expr)
  : T.expr * ty =
  match e with
  | EInt n  -> (T.TEInt n,  TyInt)
  | EBool b -> (T.TEBool b, TyBool)
  | EStringLit s ->
      (* "..." : Array[byte] — bytes live in the static region forever.
         The handle is copyable, the gen tag will always match. *)
      let result_ty = TyApp ("Array", [TyApp ("byte", [])]) in
      (T.TEStringLit s, result_ty)

  | EBinop (op, a, b) ->
      let (ta, ta_ty) = infer env tparams vars a in
      let (tb, tb_ty) = infer env tparams vars b in
      let result_ty =
        match binop_typing op with
        | OpFixed (operand_ty, result_ty) ->
            unify ta_ty operand_ty;
            unify tb_ty operand_ty;
            result_ty
        | OpEqual ->
            unify ta_ty tb_ty;
            (match prune ta_ty with
             | TyInt | TyBool -> ()
             | TyApp ("byte", []) -> ()
             | TyMeta _ -> unify ta_ty TyInt   (* default to int *)
             | t ->
                 raise (Type_error
                   (Printf.sprintf
                      "%s requires int, bool, or byte operands, got %s"
                      (show_binop op) (show_ty (zonk t)))));
            TyBool
      in
      (T.TEBinop (op, ta, tb, result_ty), result_ty)

  | EUnop (op, e) ->
      let (te, te_ty) = infer env tparams vars e in
      let (operand_ty, result_ty) = unop_typing op in
      unify te_ty operand_ty;
      (T.TEUnop (op, te, result_ty), result_ty)

  | EVar x ->
      (match List.assoc_opt x vars with
       | Some (t, _) -> (T.TEVar (x, t), t)
       | None ->
           (match List.assoc_opt x env.fns with
            | Some (fn_tparams, (params, ret)) ->
                let (subst, metas) = make_instantiation fn_tparams in
                let params = List.map (subst_ty subst) params in
                let ret = subst_ty subst ret in
                let fn_ty = TyFun (params, ret) in
                (T.TEFnRef (x, metas, fn_ty), fn_ty)
            | None ->
                raise (Type_error
                  (Printf.sprintf "unbound variable %S" x))))

  | ECall (callee, args) ->
      let (tc, tc_ty) = infer env tparams vars callee in
      let fn_ty = prune tc_ty in
      let (arg_tys, ret_ty) =
        match fn_ty with
        | TyFun (a, r) -> (a, r)
        | TyMeta _ ->
            (* peg unknown callee type to a function of the right arity *)
            let metas = List.map (fun _ -> TyMeta (fresh_meta ())) args in
            let ret_meta = TyMeta (fresh_meta ()) in
            unify fn_ty (TyFun (metas, ret_meta));
            (metas, ret_meta)
        | _ ->
            raise (Type_error
              (Printf.sprintf
                 "callee has type %s, expected a function"
                 (show_ty (zonk fn_ty))))
      in
      let callee_label = match callee with
        | EVar n -> n
        | _      -> "<call>"
      in
      let typed_args =
        check_args env tparams vars callee_label arg_tys args
      in
      (match tc with
       | T.TEFnRef (_, metas, _) ->
           check_instantiation
             (Printf.sprintf "call to %S" callee_label) metas
       | _ -> ());
      (T.TECall (tc, typed_args, ret_ty), ret_ty)

  | ECtor (c, args) ->
      let info =
        try List.assoc c env.ctors
        with Not_found ->
          raise (Type_error
            (Printf.sprintf "unknown constructor %S" c))
      in
      let (subst, metas) = make_instantiation info.ctor_owner_params in
      let arg_tys = List.map (subst_ty subst) info.ctor_args in
      let owner_tys = List.map (subst_ty subst)
        (List.map (fun p -> TyVar p) info.ctor_owner_params)
      in
      let result_ty = TyApp (info.ctor_owner, owner_tys) in
      let typed_args = check_args env tparams vars c arg_tys args in
      check_instantiation (Printf.sprintf "constructor %S" c) metas;
      (T.TECtor (c, metas, typed_args, result_ty), result_ty)

  | ERecord (name, elems) ->
      let rd =
        try List.assoc name env.records
        with Not_found ->
          raise (Type_error
            (Printf.sprintf "unknown record type %S" name))
      in
      let (subst, metas) =
        make_instantiation rd.rec_type_params
      in
      let owner_tys = List.map (subst_ty subst)
        (List.map (fun p -> TyVar p) rd.rec_type_params)
      in
      let result_ty = TyApp (name, owner_tys) in
      let declared_fields = rd.rec_fields in
      let declared_names = List.map fst declared_fields in
      (* Walk init elements left to right; each one either sets one
         field (RAssign) or expands to setting every field (RSpread).
         Later assignments win — this is C99 designated-initializer
         semantics. *)
      let field_map : (string, T.expr) Hashtbl.t = Hashtbl.create 8 in
      let spread_bindings = ref [] in
      let spread_counter = ref 0 in
      List.iter (fun elem ->
        match elem with
        | RAssign (fname, value) ->
            let decl_ty =
              try List.assoc fname declared_fields
              with Not_found ->
                raise (Type_error
                  (Printf.sprintf "record %S has no field %S" name fname))
            in
            let expected = subst_ty subst decl_ty in
            let (tv, tv_ty) = infer env tparams vars value in
            (try unify expected tv_ty
             with Type_error _ ->
               raise (Type_error
                 (Printf.sprintf
                    "field %S of %S: expected %s, got %s"
                    fname name (show_ty (zonk expected))
                    (show_ty (zonk tv_ty)))));
            Hashtbl.replace field_map fname tv
        | RSpread base ->
            let (tb, tb_ty) = infer env tparams vars base in
            (try unify result_ty tb_ty
             with Type_error _ ->
               raise (Type_error
                 (Printf.sprintf
                    "spread base in %S literal: expected %s, got %s"
                    name (show_ty (zonk result_ty))
                    (show_ty (zonk tb_ty)))));
            incr spread_counter;
            let var_name =
              Printf.sprintf "_spread_%d" !spread_counter in
            spread_bindings := (var_name, result_ty, tb) :: !spread_bindings;
            List.iter (fun (fname, decl_ty) ->
              let expected = subst_ty subst decl_ty in
              let access =
                T.TEField (T.TEVar (var_name, result_ty), fname, expected)
              in
              Hashtbl.replace field_map fname access)
              declared_fields
      ) elems;
      List.iter (fun fname ->
        if not (Hashtbl.mem field_map fname) then
          raise (Type_error
            (Printf.sprintf "missing field %S in %S literal" fname name)))
        declared_names;
      let typed_fields =
        List.map (fun (fname, _) ->
          (fname, Hashtbl.find field_map fname)) declared_fields
      in
      check_instantiation (Printf.sprintf "record %S literal" name) metas;
      let record_expr =
        T.TERecord (name, metas, typed_fields, result_ty)
      in
      (* Wrap the literal in `let _spread_N = base_N in ...` for each
         spread, so the base is evaluated exactly once even if its
         fields are accessed many times. Bindings list was cons'd in
         source order; fold_left wraps inner-first, so spread_1 ends
         up as the outermost let (evaluated first), spread_N innermost. *)
      let wrapped =
        List.fold_left (fun acc (var_name, var_ty, value) ->
          T.TELet (var_name, var_ty, value, acc, result_ty, false))
          record_expr !spread_bindings
      in
      (wrapped, result_ty)

  | EField (e, fname) ->
      let (te, te_ty) = infer env tparams vars e in
      let te_ty_now = prune te_ty in
      (match te_ty_now with
       | TyApp (n, args) when List.mem_assoc n env.records ->
           let rd = List.assoc n env.records in
           let decl_field_ty =
             try List.assoc fname rd.rec_fields
             with Not_found ->
               raise (Type_error
                 (Printf.sprintf "record %S has no field %S" n fname))
           in
           let subst = List.combine rd.rec_type_params args in
           let field_ty = subst_ty subst decl_field_ty in
           (T.TEField (te, fname, field_ty), field_ty)
       | _ ->
           raise (Type_error
             (Printf.sprintf
                "field access on non-record: expected a record, got %s"
                (show_ty (zonk te_ty)))))

  | EIf (cond, then_b, else_b) ->
      let (tc, tc_ty) = infer env tparams vars cond in
      unify tc_ty TyBool;
      let (tt, tt_ty) = infer env tparams vars then_b in
      let (te, te_ty) = infer env tparams vars else_b in
      unify tt_ty te_ty;
      (T.TEIf (tc, tt, te, tt_ty), tt_ty)

  | ELet (x, is_mut, ascription, value, body) ->
      if x <> "_" then check_not_c_reserved "let-binding" x;
      let (tv, tv_ty) = infer env tparams vars value in
      (match ascription with
       | None -> ()
       | Some t ->
           let t = validate_ty_for_ascription env tparams t in
           unify tv_ty t);
      (* Linear types (Region, user `linear` structs/enums) cannot be
         `mut` — reassigning would silently leak the previous value. *)
      if is_mut && is_linear_ty tv_ty then
        raise (Type_error
          (Printf.sprintf
             "let mut %s : %s is forbidden — \
              reassigning a linear binding would leak the previous value. \
              Create a fresh let-binding instead."
             x (show_ty (zonk tv_ty))));
      (* Linear values cannot be aliased. `let y = x` where x is linear
         would silently create two owners of the same resource. *)
      (match tv, prune tv_ty with
       | T.TEVar (src, _), t when is_linear_ty t ->
           raise (Type_error
             (Printf.sprintf
                "cannot bind one linear variable to another \
                 (let %s = %s): linear values must come from a fresh \
                 constructor or function call"
                x src))
       | _ -> ());
      (* Field access of a linear container is also aliasing — it would
         create a binding that owns the same underlying resource. *)
      (match tv, prune tv_ty with
       | T.TEField (_, fname, _), t when is_linear_ty t ->
           raise (Type_error
             (Printf.sprintf
                "cannot bind a linear field to a new name \
                 (let %s = ....%s): consume it directly with drop(...) \
                 or pass it to a function instead"
                x fname))
       | _ -> ());
      (* `_ = <diverging-expr>` (break/continue/return) — value type is
         unresolved TyMeta with nothing to constrain it. Default to int
         so zonk doesn't fail. *)
      if x = "_" then
        (match prune tv_ty with
         | TyMeta _ -> unify tv_ty TyInt
         | _ -> ());
      let x_actual =
        if x = "_" && is_linear_ty tv_ty then begin
          incr drop_name_counter;
          Printf.sprintf "_drop_%d" !drop_name_counter
        end else x
      in
      let body_vars =
        if x_actual = "_" then vars
        else (x_actual, (tv_ty, is_mut)) :: vars
      in
      let (tb, tb_ty) = infer env tparams body_vars body in
      let auto_drop =
        if x_actual = "_" then false
        else is_linear_ty tv_ty
      in
      (T.TELet (x_actual, tv_ty, tv, tb, tb_ty, auto_drop), tb_ty)

  | EAssign (x, value) ->
      let (tv, tv_ty) = infer env tparams vars value in
      (match List.assoc_opt x vars with
       | Some (xt, true) ->
           (try unify xt tv_ty
            with Type_error _ ->
              raise (Type_error
                (Printf.sprintf
                   "assignment to %S: variable has type %s, value has type %s"
                   x (show_ty (zonk xt)) (show_ty (zonk tv_ty)))));
           (T.TEAssign (x, tv, xt), TyInt)
       | Some (_, false) ->
           raise (Type_error
             (Printf.sprintf
                "cannot assign to %S — declared without `mut`. \
                 Use `let mut %s = ...` to make it reassignable."
                x x))
       | None ->
           raise (Type_error
             (Printf.sprintf "assignment to unknown variable %S" x)))

  | EWhile (cond, body) ->
      let (tc, tc_ty) = infer env tparams vars cond in
      (try unify tc_ty TyBool
       with Type_error _ ->
         raise (Type_error
           (Printf.sprintf
              "while condition must be bool, got %s"
              (show_ty (zonk tc_ty)))));
      incr loop_depth;
      let (tb, _tb_ty) = infer env tparams vars body in
      decr loop_depth;
      (* while never produces a real value; we use int 0 as placeholder
         for "unit" same as a[i] := v. *)
      (T.TEWhile (tc, tb), TyInt)

  | EBreak ->
      if !loop_depth = 0 then
        raise (Type_error "break used outside of a while loop");
      (T.TEBreak, TyMeta (fresh_meta ()))

  | EContinue ->
      if !loop_depth = 0 then
        raise (Type_error "continue used outside of a while loop");
      (T.TEContinue, TyMeta (fresh_meta ()))

  | EReturn v_e ->
      let (tv, tv_ty) = infer env tparams vars v_e in
      let ret_ty =
        match !current_return_ty with
        | Some t -> t
        | None -> failwith "check: EReturn outside of a function body"
      in
      (try unify tv_ty ret_ty
       with Type_error _ ->
         raise (Type_error
           (Printf.sprintf
              "return: expression has type %s, function returns %s"
              (show_ty (zonk tv_ty)) (show_ty (zonk ret_ty)))));
      (T.TEReturn (tv, ret_ty), TyMeta (fresh_meta ()))

  | EMatch (scrut, arms) ->
      if arms = [] then
        raise (Type_error "match must have at least one arm");
      let (tscrut, tscrut_ty) = infer env tparams vars scrut in
      let kind = scrutinee_kind env tscrut_ty in
      check_match_arms_structure env kind arms;
      let typed_arms = List.map (fun (pat, body) ->
        let body_vars =
          match pat with
          | PBind "_" -> vars
          | PBind x ->
              check_not_c_reserved "pattern bind" x;
              (x, (tscrut_ty, false)) :: vars
          | PCtor (c, vs) ->
              let info = List.assoc c env.ctors in
              if List.length vs <> List.length info.ctor_args then
                raise (Type_error
                  (Printf.sprintf
                     "constructor %S expects %d field(s), pattern has %d"
                     c (List.length info.ctor_args) (List.length vs)));
              List.iter (fun v ->
                if v <> "_" then check_not_c_reserved "pattern variable" v) vs;
              let (subst, _) =
                make_instantiation info.ctor_owner_params
              in
              let arg_tys = List.map (subst_ty subst) info.ctor_args in
              let owner_args =
                List.map (subst_ty subst)
                  (List.map (fun p -> TyVar p) info.ctor_owner_params)
              in
              let inst_result = TyApp (info.ctor_owner, owner_args) in
              unify inst_result tscrut_ty;
              List.fold_left2 (fun acc v t ->
                if v = "_" then acc else (v, (t, false)) :: acc)
                vars vs arg_tys
          | POr pats ->
              (* For ADT or-patterns: unify scrut with first ctor's
                 owner; structural check has already verified all arms
                 belong to the same ADT. For literal or-patterns
                 (PInt/PBool/PStr): scrut type already constrained by
                 scrutinee_kind, no extra unify needed. *)
              (match pats with
               | PCtor (c, _) :: _ ->
                   let info = List.assoc c env.ctors in
                   let (subst, _) =
                     make_instantiation info.ctor_owner_params
                   in
                   let owner_args =
                     List.map (subst_ty subst)
                       (List.map (fun p -> TyVar p) info.ctor_owner_params)
                   in
                   let inst_result = TyApp (info.ctor_owner, owner_args) in
                   unify inst_result tscrut_ty
               | _ -> ());
              vars
          | PInt _ | PBool _ | PStr _ -> vars
        in
        let (tbody, tbody_ty) = infer env tparams body_vars body in
        ((pat, tbody), tbody_ty)) arms
      in
      let first_ty = snd (List.hd typed_arms) in
      List.iter (fun (_, t) -> unify first_ty t) typed_arms;
      let arms_out = List.map fst typed_arms in
      (T.TEMatch (tscrut, tscrut_ty, arms_out, first_ty), first_ty)

  | EArray (region_e, size_e, init_e) ->
      (* array(r, N, v) : (Region, int, T) → Array[T].
         Bump-allocates N slots in region r, fills each with v.
         The Array wrapper is copyable — its memory lives in r. *)
      let (tr, tr_ty) = infer env tparams vars region_e in
      (try unify tr_ty (TyApp ("Region", []))
       with Type_error _ ->
         raise (Type_error
           (Printf.sprintf
              "array(r, _, _) : first argument must be Region, got %s"
              (show_ty (zonk tr_ty)))));
      let (tn, tn_ty) = infer env tparams vars size_e in
      (try unify tn_ty TyInt
       with Type_error _ ->
         raise (Type_error
           (Printf.sprintf
              "array(_, N, _) : size must be int, got %s"
              (show_ty (zonk tn_ty)))));
      let (tv, tv_ty) = infer env tparams vars init_e in
      if ty_contains_linear (zonk tv_ty) then
        raise (Type_error
          (Printf.sprintf
             "array(_, _, v) : element type cannot contain a linear type (%s)"
             (show_ty (zonk tv_ty))));
      let result_ty = TyApp ("Array", [tv_ty]) in
      (T.TEArray (tr, tn, tv, result_ty), result_ty)

  | EArrayLit (region_e, elems) ->
      (* array(r, [v0, ..., vN-1]) : Region, expr list → Array[T].
         All values must share one element type; result length = list length. *)
      if elems = [] then
        raise (Type_error
          "array(r, []) requires at least one element to infer type — \
           use array(r, 0, default) for an empty array");
      let (tr, tr_ty) = infer env tparams vars region_e in
      (try unify tr_ty (TyApp ("Region", []))
       with Type_error _ ->
         raise (Type_error
           (Printf.sprintf
              "array(r, [..]) : first argument must be Region, got %s"
              (show_ty (zonk tr_ty)))));
      let typed_elems = List.map (infer env tparams vars) elems in
      let elem_ty = snd (List.hd typed_elems) in
      List.iter (fun (_, t) ->
        (try unify elem_ty t
         with Type_error _ ->
           raise (Type_error
             (Printf.sprintf
                "array literal elements must all have the same type: \
                 expected %s, got %s"
                (show_ty (zonk elem_ty)) (show_ty (zonk t)))))) typed_elems;
      if ty_contains_linear (zonk elem_ty) then
        raise (Type_error
          (Printf.sprintf
             "array literal element type cannot contain a linear type (%s)"
             (show_ty (zonk elem_ty))));
      let result_ty = TyApp ("Array", [elem_ty]) in
      (T.TEArrayLit (tr, List.map fst typed_elems, result_ty), result_ty)

  | ERegion size_e ->
      (* region(N) : int → Region. Heap arena. Linear. *)
      let (tn, tn_ty) = infer env tparams vars size_e in
      (try unify tn_ty TyInt
       with Type_error _ ->
         raise (Type_error
           (Printf.sprintf
              "region(N) : size must be int, got %s"
              (show_ty (zonk tn_ty)))));
      let result_ty = TyApp ("Region", []) in
      (T.TERegion (tn, result_ty), result_ty)

  | EStackRegion size_e ->
      (* stack_region(N) : int → Region. Block on stack of current
         C function. N must be a literal (parser already enforced). *)
      let (tn, tn_ty) = infer env tparams vars size_e in
      (try unify tn_ty TyInt
       with Type_error _ ->
         raise (Type_error
           (Printf.sprintf
              "stack_region(N) : size must be int, got %s"
              (show_ty (zonk tn_ty)))));
      let result_ty = TyApp ("Region", []) in
      (T.TEStackRegion (tn, result_ty), result_ty)

  | EAlignedRegion (size_e, align_e) ->
      (* aligned_region(N, A) : int * int → Region. Heap arena with
         the block aligned to A bytes (for mmap, GPU, DMA). A must be
         a positive power-of-two literal (parser already enforced). *)
      let (tn, tn_ty) = infer env tparams vars size_e in
      (try unify tn_ty TyInt
       with Type_error _ ->
         raise (Type_error
           (Printf.sprintf
              "aligned_region(N, _) : size must be int, got %s"
              (show_ty (zonk tn_ty)))));
      let (ta, ta_ty) = infer env tparams vars align_e in
      (try unify ta_ty TyInt
       with Type_error _ ->
         raise (Type_error
           (Printf.sprintf
              "aligned_region(_, A) : alignment must be int, got %s"
              (show_ty (zonk ta_ty)))));
      let result_ty = TyApp ("Region", []) in
      (T.TEAlignedRegion (tn, ta, result_ty), result_ty)

  | EIndex (arr_e, idx_e) ->
      (* a[i] : Array[T] or *T, int → T. For Array, the read does a
         gen+bounds check; for raw *T it's a plain C subscript. *)
      let (ta, ta_ty) = infer env tparams vars arr_e in
      let elem =
        match prune ta_ty with
        | TyPtr inner -> inner
        | _ ->
            let elem = TyMeta (fresh_meta ()) in
            (try unify ta_ty (TyApp ("Array", [elem]))
             with Type_error _ ->
               raise (Type_error
                 (Printf.sprintf
                    "indexing expects Array[T] or *T, got %s"
                    (show_ty (zonk ta_ty)))));
            elem
      in
      let (ti, ti_ty) = infer env tparams vars idx_e in
      (try unify ti_ty TyInt
       with Type_error _ ->
         raise (Type_error
           (Printf.sprintf
              "index must be int, got %s"
              (show_ty (zonk ti_ty)))));
      (T.TEIndex (ta, ti, elem), elem)

  | EAssignIdx (arr_e, idx_e, val_e) ->
      let (ta, ta_ty) = infer env tparams vars arr_e in
      let elem =
        match prune ta_ty with
        | TyPtr inner -> inner
        | _ ->
            let elem = TyMeta (fresh_meta ()) in
            (try unify ta_ty (TyApp ("Array", [elem]))
             with Type_error _ ->
               raise (Type_error
                 (Printf.sprintf
                    "index-assignment expects Array[T] or *T, got %s"
                    (show_ty (zonk ta_ty)))));
            elem
      in
      let (ti, ti_ty) = infer env tparams vars idx_e in
      (try unify ti_ty TyInt
       with Type_error _ ->
         raise (Type_error
           (Printf.sprintf
              "index must be int, got %s"
              (show_ty (zonk ti_ty)))));
      let (tv, tv_ty) = infer env tparams vars val_e in
      (try unify tv_ty elem
       with Type_error _ ->
         raise (Type_error
           (Printf.sprintf
              "type mismatch in a[i] := v: element is %s, value is %s"
              (show_ty (zonk elem)) (show_ty (zonk tv_ty)))));
      (T.TEAssignIdx (ta, ti, tv, TyInt), TyInt)

  | ELen arr_e ->
      (* len(a) : Array[T] → int. *)
      let (ta, ta_ty) = infer env tparams vars arr_e in
      let elem = TyMeta (fresh_meta ()) in
      (try unify ta_ty (TyApp ("Array", [elem]))
       with Type_error _ ->
         raise (Type_error
           (Printf.sprintf
              "len expects Array[T], got %s"
              (show_ty (zonk ta_ty)))));
      (T.TELen (ta, TyInt), TyInt)

  | ESlice (arr_e, lo_e, hi_e) ->
      (* slice(a, lo, hi) : Array[T], int, int → Array[T]. New handle
         pointing at the same region, with offset += lo and len = hi - lo.
         Same gen, same slot — slice dies with the original region. *)
      let (ta, ta_ty) = infer env tparams vars arr_e in
      let elem = TyMeta (fresh_meta ()) in
      (try unify ta_ty (TyApp ("Array", [elem]))
       with Type_error _ ->
         raise (Type_error
           (Printf.sprintf
              "slice expects Array[T], got %s"
              (show_ty (zonk ta_ty)))));
      let (tlo, tlo_ty) = infer env tparams vars lo_e in
      (try unify tlo_ty TyInt
       with Type_error _ ->
         raise (Type_error
           (Printf.sprintf
              "slice(_, lo, _) : lo must be int, got %s"
              (show_ty (zonk tlo_ty)))));
      let (thi, thi_ty) = infer env tparams vars hi_e in
      (try unify thi_ty TyInt
       with Type_error _ ->
         raise (Type_error
           (Printf.sprintf
              "slice(_, _, hi) : hi must be int, got %s"
              (show_ty (zonk thi_ty)))));
      let result_ty = TyApp ("Array", [elem]) in
      (T.TESlice (ta, tlo, thi, result_ty), result_ty)

  | EToInt sub_e ->
      let (ts, ts_ty) = infer env tparams vars sub_e in
      (try unify ts_ty (TyApp ("byte", []))
       with Type_error _ ->
         raise (Type_error
           (Printf.sprintf
              "to_int expects byte, got %s"
              (show_ty (zonk ts_ty)))));
      (T.TEToInt ts, TyInt)

  | EToByte sub_e ->
      let (ts, ts_ty) = infer env tparams vars sub_e in
      (try unify ts_ty TyInt
       with Type_error _ ->
         raise (Type_error
           (Printf.sprintf
              "to_byte expects int, got %s"
              (show_ty (zonk ts_ty)))));
      (T.TEToByte ts, TyApp ("byte", []))

  | ECAlloc (elem_t, n_e) ->
      let elem_t = validate_ty_for_ascription env tparams elem_t in
      if ty_contains_linear elem_t then
        raise (Type_error
          (Printf.sprintf
             "c_alloc[T](_) : T cannot contain a linear type (%s)"
             (show_ty (zonk elem_t))));
      let (tn, tn_ty) = infer env tparams vars n_e in
      (try unify tn_ty TyInt
       with Type_error _ ->
         raise (Type_error
           (Printf.sprintf
              "c_alloc[_](N) : N must be int, got %s"
              (show_ty (zonk tn_ty)))));
      let result_ty = TyPtr elem_t in
      (T.TECAlloc (elem_t, tn, result_ty), result_ty)

  | ECFree p_e ->
      let (tp, tp_ty) = infer env tparams vars p_e in
      (match prune tp_ty with
       | TyPtr _ -> ()
       | TyMeta _ ->
           let elem = TyMeta (fresh_meta ()) in
           unify tp_ty (TyPtr elem)
       | _ ->
           raise (Type_error
             (Printf.sprintf
                "c_free expects a raw pointer, got %s"
                (show_ty (zonk tp_ty)))));
      (T.TECFree tp, TyInt)

  | ENullPtr elem_t ->
      let elem_t = validate_ty_for_ascription env tparams elem_t in
      let result_ty = TyPtr elem_t in
      (T.TENullPtr result_ty, result_ty)

  | EIsNull p_e ->
      let (tp, tp_ty) = infer env tparams vars p_e in
      (match prune tp_ty with
       | TyPtr _ -> ()
       | TyMeta _ ->
           let elem = TyMeta (fresh_meta ()) in
           unify tp_ty (TyPtr elem)
       | _ ->
           raise (Type_error
             (Printf.sprintf
                "is_null expects a raw pointer, got %s"
                (show_ty (zonk tp_ty)))));
      (T.TEIsNull tp, TyBool)

  | EArrayData a_e ->
      let (ta, ta_ty) = infer env tparams vars a_e in
      let elem = TyMeta (fresh_meta ()) in
      (try unify ta_ty (TyApp ("Array", [elem]))
       with Type_error _ ->
         raise (Type_error
           (Printf.sprintf
              "array_data expects Array[T], got %s"
              (show_ty (zonk ta_ty)))));
      let result_ty = TyPtr elem in
      (T.TEArrayData (ta, result_ty), result_ty)

  | EDeref p_e ->
      let (tp, tp_ty) = infer env tparams vars p_e in
      let elem = TyMeta (fresh_meta ()) in
      (try unify tp_ty (TyPtr elem)
       with Type_error _ ->
         raise (Type_error
           (Printf.sprintf
              "*p expects a raw pointer, got %s"
              (show_ty (zonk tp_ty)))));
      (T.TEDeref (tp, elem), elem)

  | ETryAt (a_e, i_e) ->
      let (ta, ta_ty) = infer env tparams vars a_e in
      let elem = TyMeta (fresh_meta ()) in
      (try unify ta_ty (TyApp ("Array", [elem]))
       with Type_error _ ->
         raise (Type_error
           (Printf.sprintf
              "try_at expects Array[T], got %s"
              (show_ty (zonk ta_ty)))));
      let (ti, ti_ty) = infer env tparams vars i_e in
      (try unify ti_ty TyInt
       with Type_error _ ->
         raise (Type_error
           (Printf.sprintf
              "try_at(_, i): i must be int, got %s"
              (show_ty (zonk ti_ty)))));
      let result_ty = TyApp ("Option", [elem]) in
      (T.TETryAt (ta, ti, result_ty), result_ty)

  | EDrop x_e ->
      let (tx, tx_ty) = infer env tparams vars x_e in
      if not (is_linear_ty tx_ty) then
        raise (Type_error
          (Printf.sprintf
             "drop() requires a linear value (Region or a user `linear` \
              type), got %s"
             (show_ty (zonk tx_ty))));
      (T.TEDrop (tx, tx_ty), TyInt)

and check_args env tparams vars callee_name param_tys args : T.expr list =
  let n_expected = List.length param_tys in
  let n_got = List.length args in
  if n_expected <> n_got then
    raise (Type_error
      (Printf.sprintf "%S expects %d argument(s), got %d"
         callee_name n_expected n_got));
  List.map2 (fun expected arg ->
    let (targ, t) = infer env tparams vars arg in
    (try unify expected t
     with Type_error _ ->
       raise (Type_error
         (Printf.sprintf
            "argument type mismatch in %S: expected %s, got %s"
            callee_name (show_ty (zonk expected)) (show_ty (zonk t)))));
    targ) param_tys args

and validate_ty_for_ascription
  (env : env) (tparams : string list) (t : ty) : ty =
  (* Same validation as during build_env, just routed through the env
     record (which already holds type_env and record_env). *)
  validate_ty env.types env.records tparams t

(* ---------- zonk the typed AST ---------- *)

let rec zonk_expr (e : T.expr) : T.expr =
  match e with
  | T.TEInt _ | T.TEBool _ | T.TEStringLit _ -> e
  | T.TEVar (x, t) -> T.TEVar (x, zonk_expect t)
  | T.TEFnRef (name, ts, fn_ty) ->
      T.TEFnRef (name, List.map zonk_expect ts, zonk_expect fn_ty)
  | T.TECall (callee, args, ret) ->
      T.TECall (zonk_expr callee,
                List.map zonk_expr args, zonk_expect ret)
  | T.TEBinop (op, a, b, ty) ->
      T.TEBinop (op, zonk_expr a, zonk_expr b, zonk_expect ty)
  | T.TEUnop (op, e, ty) ->
      T.TEUnop (op, zonk_expr e, zonk_expect ty)
  | T.TECtor (c, ts, args, ret) ->
      T.TECtor (c, List.map zonk_expect ts,
                List.map zonk_expr args, zonk_expect ret)
  | T.TERecord (name, ts, fields, ret) ->
      T.TERecord (name, List.map zonk_expect ts,
                  List.map (fun (fn, e) -> (fn, zonk_expr e)) fields,
                  zonk_expect ret)
  | T.TEField (e, fname, ty) ->
      T.TEField (zonk_expr e, fname, zonk_expect ty)
  | T.TEIf (c, t, e, ty) ->
      T.TEIf (zonk_expr c, zonk_expr t, zonk_expr e, zonk_expect ty)
  | T.TELet (x, vt, v, b, bt, ad) ->
      T.TELet (x, zonk_expect vt, zonk_expr v,
               zonk_expr b, zonk_expect bt, ad)
  | T.TEMatch (s, st, arms, rt) ->
      let arms = List.map (fun (p, b) -> (p, zonk_expr b)) arms in
      T.TEMatch (zonk_expr s, zonk_expect st, arms, zonk_expect rt)
  | T.TEArray (r, n, v, t) ->
      T.TEArray (zonk_expr r, zonk_expr n, zonk_expr v, zonk_expect t)
  | T.TEArrayLit (r, elems, t) ->
      T.TEArrayLit (zonk_expr r, List.map zonk_expr elems, zonk_expect t)
  | T.TERegion (n, t) -> T.TERegion (zonk_expr n, zonk_expect t)
  | T.TEStackRegion (n, t) ->
      T.TEStackRegion (zonk_expr n, zonk_expect t)
  | T.TEAlignedRegion (n, a, t) ->
      T.TEAlignedRegion (zonk_expr n, zonk_expr a, zonk_expect t)
  | T.TEIndex (a, i, t) ->
      T.TEIndex (zonk_expr a, zonk_expr i, zonk_expect t)
  | T.TEAssignIdx (a, i, v, t) ->
      T.TEAssignIdx (zonk_expr a, zonk_expr i, zonk_expr v, zonk_expect t)
  | T.TELen (e, t) ->
      T.TELen (zonk_expr e, zonk_expect t)
  | T.TESlice (a, lo, hi, t) ->
      T.TESlice (zonk_expr a, zonk_expr lo, zonk_expr hi, zonk_expect t)
  | T.TEToInt e  -> T.TEToInt (zonk_expr e)
  | T.TEToByte e -> T.TEToByte (zonk_expr e)
  | T.TECAlloc (et, n, rt) ->
      T.TECAlloc (zonk_expect et, zonk_expr n, zonk_expect rt)
  | T.TECFree e -> T.TECFree (zonk_expr e)
  | T.TENullPtr t -> T.TENullPtr (zonk_expect t)
  | T.TEIsNull e -> T.TEIsNull (zonk_expr e)
  | T.TEArrayData (a, t) -> T.TEArrayData (zonk_expr a, zonk_expect t)
  | T.TEDeref (p, t) -> T.TEDeref (zonk_expr p, zonk_expect t)
  | T.TEAssign (x, v, t) -> T.TEAssign (x, zonk_expr v, zonk_expect t)
  | T.TEWhile (c, b) -> T.TEWhile (zonk_expr c, zonk_expr b)
  | T.TEBreak | T.TEContinue -> e
  | T.TEReturn (v, t) -> T.TEReturn (zonk_expr v, zonk_expect t)
  | T.TETryAt (a, i, t) ->
      T.TETryAt (zonk_expr a, zonk_expr i, zonk_expect t)
  | T.TEDrop (e, t) ->
      T.TEDrop (zonk_expr e, zonk_expect t)

and zonk_expect (t : ty) : ty =
  let t = zonk t in
  let rec has_unresolved = function
    | TyInt | TyBool | TyVar _ -> false
    | TyApp (_, args) -> List.exists has_unresolved args
    | TyFun (args, ret) ->
        List.exists has_unresolved args || has_unresolved ret
    | TyPtr inner -> has_unresolved inner
    | TyMeta _ -> true
  in
  if has_unresolved t then
    raise (Type_error
      (Printf.sprintf
         "could not infer type (still %s after checking) — \
          consider adding a `let x: SomeType = ...` annotation"
         (show_ty t)))
  else t

(* ---------- move check ----------

   Walks the typed AST tracking which names are still live (not yet
   moved). A bare-EVar use of a non-copyable name in a move-position
   (let RHS, fn arg, ctor arg, record field value, unwrap/take arg)
   removes that name from the live set. Subsequent use of a dead name
   is a compile error. Branches of if/match must end with the same
   live set — divergence means the program would leak in one path or
   double-free in another, so we reject it and require the user to
   write symmetric branches. *)

(* Tracks live names with their types. Type is needed because when an
   inner binding shadows an outer name we need to restore the outer
   type on scope exit. *)
module SM = Map.Make (String)

let rec check_moves_expr (env : env) (live : ty SM.t) (in_tail : bool) (e : T.expr)
  : T.expr * ty SM.t =
  match e with
  | T.TEInt _ | T.TEBool _ | T.TEStringLit _ | T.TEFnRef _ -> (e, live)

  | T.TEVar (x, t) ->
      if not (SM.mem x live) then
        raise (Type_error
          (Printf.sprintf
             "use of moved name %S — its ownership was transferred earlier"
             x));
      let live' =
        if in_tail && is_linear_ty t then SM.remove x live
        else live
      in
      (e, live')

  | T.TEField (sub, fname, ty) ->
      let (sub', live) = check_moves_expr env live false sub in
      (T.TEField (sub', fname, ty), live)

  | T.TEBinop (op, a, b, ty) ->
      let (a', live) = check_moves_expr env live false a in
      (match op with
       | OpAnd | OpOr ->
           let (b', live_b) = check_moves_expr env live false b in
           if not (SM.equal (fun _ _ -> true) live_b live) then
             raise (Type_error
               (Printf.sprintf
                  "right operand of %s diverges in ownership" (show_binop op)));
           (T.TEBinop (op, a', b', ty), live)
       | _ ->
           let (b', live) = check_moves_expr env live false b in
           (T.TEBinop (op, a', b', ty), live))

  | T.TEUnop (op, sub, ty) ->
      let (sub', live) = check_moves_expr env live false sub in
      (T.TEUnop (op, sub', ty), live)

  | T.TECall (callee, args, ty) ->
      let (callee', live) = check_moves_expr env live false callee in
      let (args_rev, live) = List.fold_left (fun (acc, live) a ->
        let (a', live) = check_moves_expr env live false a in
        (a' :: acc, live)) ([], live) args
      in
      (T.TECall (callee', List.rev args_rev, ty), live)

  | T.TECtor (c, ts, args, ty) ->
      let (args_rev, live) = List.fold_left (fun (acc, live) a ->
        let (a', live) = check_moves_expr env live false a in
        (a' :: acc, live)) ([], live) args
      in
      (T.TECtor (c, ts, List.rev args_rev, ty), live)

  | T.TERecord (n, ts, fields, ty) ->
      let (fields_rev, live) = List.fold_left (fun (acc, live) (f, e) ->
        let (e', live) = check_moves_expr env live false e in
        ((f, e') :: acc, live)) ([], live) fields
      in
      (T.TERecord (n, ts, List.rev fields_rev, ty), live)

  | T.TEIf (cond, t, el, ty) ->
      let (cond', live) = check_moves_expr env live false cond in
      let (t', live_t) = check_moves_expr env live in_tail t in
      let (el', live_e) = check_moves_expr env live in_tail el in
      if not (SM.equal (fun _ _ -> true) live_t live_e) then
        raise (Type_error
          (Printf.sprintf
             "if branches diverge in ownership: \
              then leaves [%s] live, else leaves [%s] live. \
              Both branches must end with the same ownership state. \
              Use `let _ = x` to consume a linear value in a branch that \
              doesn't otherwise use it."
             (String.concat ", " (List.map fst (SM.bindings live_t)))
             (String.concat ", " (List.map fst (SM.bindings live_e)))));
      (T.TEIf (cond', t', el', ty), live_t)

  | T.TELet (x, vt, v, b, bt, ad) ->
      let (v', live) = check_moves_expr env live false v in
      if x = "_" then
        let (b', live) = check_moves_expr env live in_tail b in
        (T.TELet ("_", vt, v', b', bt, ad), live)
      else
        let outer_had = SM.find_opt x live in
        let live_inner = SM.add x vt live in
        let (b', live_after) = check_moves_expr env live_inner in_tail b in
        (* If x was consumed somewhere in the body (drop, tail-return),
           it's no longer in live_after — skip the scope-end auto-drop
           to avoid double-free. *)
        let ad' = if ad && not (SM.mem x live_after) then false else ad in
        let live_final = match outer_had with
          | Some t -> SM.add x t live_after
          | None -> SM.remove x live_after
        in
        (T.TELet (x, vt, v', b', bt, ad'), live_final)

  | T.TEMatch (scrut, scrut_ty, arms, ty) ->
      let (scrut', live) = check_moves_expr env live false scrut in
      (* Compute binding types for each pattern by substituting the
         scrutinee's concrete type arguments into the ctor's arg types. *)
      let arm_data = List.map (fun (pat, body) ->
        let names_tys = match pat with
          | POr _ -> []
          | PInt _ | PBool _ | PStr _ -> []
          | PBind "_" -> []
          | PBind x -> [(x, scrut_ty)]
          | PCtor (c, vs) ->
              let info = List.assoc c env.ctors in
              let scrut_now = prune scrut_ty in
              let subst = match scrut_now with
                | TyApp (_, args) ->
                    List.combine info.ctor_owner_params args
                | _ -> []
              in
              let arg_tys =
                List.map (subst_ty subst) info.ctor_args
              in
              List.filter (fun (v, _) -> v <> "_")
                (List.combine vs arg_tys)
        in
        let outer_had =
          List.map (fun (v, _) -> (v, SM.find_opt v live)) names_tys
        in
        let live_arm =
          List.fold_left (fun l (v, t) -> SM.add v t l) live names_tys
        in
        let (body', live_after) =
          check_moves_expr env live_arm in_tail body
        in
        let live_after_restore = List.fold_left (fun l (v, prev) ->
          match prev with
          | Some t -> SM.add v t l
          | None -> SM.remove v l) live_after outer_had
        in
        (pat, body', live_after_restore)
      ) arms in
      (match arm_data with
       | [] -> (T.TEMatch (scrut', scrut_ty, [], ty), live)
       | (_, _, first_live) :: rest ->
           List.iteri (fun i (_, _, l) ->
             if not (SM.equal (fun _ _ -> true) first_live l) then
               raise (Type_error
                 (Printf.sprintf
                    "match arm #%d diverges from arm #1 in ownership: \
                     arm #1 leaves [%s] live, arm #%d leaves [%s] live"
                    (i + 2)
                    (String.concat ", " (List.map fst (SM.bindings first_live)))
                    (i + 2)
                    (String.concat ", " (List.map fst (SM.bindings l))))))
             rest;
           let arms' = List.map (fun (p, b, _) -> (p, b)) arm_data in
           (T.TEMatch (scrut', scrut_ty, arms', ty), first_live))

  | T.TEArray (r, n, v, ty) ->
      let (r', live) = check_moves_expr env live false r in
      let (n', live) = check_moves_expr env live false n in
      let (v', live) = check_moves_expr env live false v in
      (T.TEArray (r', n', v', ty), live)

  | T.TEArrayLit (r, elems, ty) ->
      let (r', live) = check_moves_expr env live false r in
      let (elems_rev, live) = List.fold_left (fun (acc, l) e ->
        let (e', l) = check_moves_expr env l false e in
        (e' :: acc, l)) ([], live) elems
      in
      (T.TEArrayLit (r', List.rev elems_rev, ty), live)

  | T.TERegion (n, ty) ->
      let (n', live) = check_moves_expr env live false n in
      (T.TERegion (n', ty), live)

  | T.TEStackRegion (n, ty) ->
      let (n', live) = check_moves_expr env live false n in
      (T.TEStackRegion (n', ty), live)

  | T.TEAlignedRegion (n, a, ty) ->
      let (n', live) = check_moves_expr env live false n in
      let (a', live) = check_moves_expr env live false a in
      (T.TEAlignedRegion (n', a', ty), live)

  | T.TEIndex (a, i, ty) ->
      let (a', live) = check_moves_expr env live false a in
      let (i', live) = check_moves_expr env live false i in
      (T.TEIndex (a', i', ty), live)

  | T.TEAssignIdx (a, i, v, ty) ->
      let (a', live) = check_moves_expr env live false a in
      let (i', live) = check_moves_expr env live false i in
      let (v', live) = check_moves_expr env live false v in
      (T.TEAssignIdx (a', i', v', ty), live)

  | T.TELen (sub, ty) ->
      let (sub', live) = check_moves_expr env live false sub in
      (T.TELen (sub', ty), live)

  | T.TESlice (a, lo, hi, ty) ->
      let (a', live)  = check_moves_expr env live false a in
      let (lo', live) = check_moves_expr env live false lo in
      let (hi', live) = check_moves_expr env live false hi in
      (T.TESlice (a', lo', hi', ty), live)

  | T.TEToInt sub ->
      let (sub', live) = check_moves_expr env live false sub in
      (T.TEToInt sub', live)

  | T.TEToByte sub ->
      let (sub', live) = check_moves_expr env live false sub in
      (T.TEToByte sub', live)

  | T.TECAlloc (et, n, rt) ->
      let (n', live) = check_moves_expr env live false n in
      (T.TECAlloc (et, n', rt), live)

  | T.TECFree p ->
      let (p', live) = check_moves_expr env live false p in
      (T.TECFree p', live)

  | T.TENullPtr _ -> (e, live)

  | T.TEIsNull p ->
      let (p', live) = check_moves_expr env live false p in
      (T.TEIsNull p', live)

  | T.TEArrayData (a, t) ->
      let (a', live) = check_moves_expr env live false a in
      (T.TEArrayData (a', t), live)

  | T.TEDeref (p, t) ->
      let (p', live) = check_moves_expr env live false p in
      (T.TEDeref (p', t), live)

  | T.TEAssign (x, v, t) ->
      let (v', live) = check_moves_expr env live false v in
      (T.TEAssign (x, v', t), live)

  | T.TEWhile (c, b) ->
      let (c', live) = check_moves_expr env live false c in
      let (b', live) = check_moves_expr env live false b in
      (T.TEWhile (c', b'), live)

  | T.TEBreak | T.TEContinue -> (e, live)

  | T.TEReturn (v, t) ->
      let (v', live) = check_moves_expr env live false v in
      (T.TEReturn (v', t), live)

  | T.TETryAt (a, i, t) ->
      let (a', live) = check_moves_expr env live false a in
      let (i', live) = check_moves_expr env live false i in
      (T.TETryAt (a', i', t), live)

  | T.TEDrop (sub, t) ->
      let (sub', live) = check_moves_expr env live false sub in
      (* Explicit drop of a bare variable consumes it. After drop, the
         name is no longer live (use-after-drop is a compile error). *)
      let live = match sub' with
        | T.TEVar (x, _) when is_linear_ty t -> SM.remove x live
        | _ -> live
      in
      (T.TEDrop (sub', t), live)

(* ---------- check a function ---------- *)

let check_func (env : env) (f : func) : T.func =
  let (_, (param_tys, ret_ty)) = List.assoc f.name env.fns in
  loop_depth := 0;
  current_return_ty := Some ret_ty;
  let vars =
    List.combine (List.map fst f.params)
      (List.map (fun t -> (t, false)) param_tys)
  in
  let tparams = f.type_params in
  let (tbody, tbody_ty) = infer env tparams vars f.body in
  current_return_ty := None;
  (try unify ret_ty tbody_ty
   with Type_error _ ->
     raise (Type_error
       (Printf.sprintf
          "function %S: body has type %s, declared return type is %s"
          f.name (show_ty (zonk tbody_ty)) (show_ty (zonk ret_ty)))));
  let tbody = zonk_expr tbody in
  let param_tys = List.map zonk param_tys in
  (* Linear params (Region and user `linear` types) are borrowed from
     the caller — caller's creating scope frees them. Callees never
     drop received linear values, so there is no per-param drop logic
     in the typed AST. *)
  let initial_live =
    List.fold_left2 (fun m (p, _) t -> SM.add p t m)
      SM.empty f.params param_tys
  in
  let (body_with_moves, _final_live) =
    check_moves_expr env initial_live true tbody
  in
  { T.name = f.name;
    T.type_params = f.type_params;
    T.params = List.combine
      (List.map fst f.params) param_tys;
    T.return_ty = ret_ty;
    T.body = body_with_moves }

(* ---------- top-level entry ---------- *)

(* Built-in Option type — privileged. The Ref operations produce it,
   so it must exist regardless of what the user declares. Same shape as
   `enum Option[T] { Some(T), None }` written by hand, just injected by
   the compiler. Users can declare neither `Option` nor `Some`/`None`. *)
let builtin_option_decl : type_decl = {
  type_name   = "Option";
  type_params = ["T"];
  variants = [
    { ctor_name = "Some"; arg_tys = [TyVar "T"] };
    { ctor_name = "None"; arg_tys = [] };
  ];
  is_linear = false;
}

let check (prog : program) : T.program =
  meta_counter := 0;
  drop_name_counter := 0;
  let (types, records, funcs, externs) = split_program prog in
  (* Reject any user attempt to redeclare reserved built-in names. *)
  List.iter (fun (td : type_decl) ->
    if td.type_name = "Option" || td.type_name = "Ref" then
      raise (Type_error
        (Printf.sprintf
           "%S is a reserved built-in type and cannot be redeclared"
           td.type_name))) types;
  List.iter (fun (rd : record_decl) ->
    if rd.rec_name = "Option" || rd.rec_name = "Ref" then
      raise (Type_error
        (Printf.sprintf
           "%S is a reserved built-in type and cannot be redeclared"
           rd.rec_name))) records;
  List.iter (fun (td : type_decl) ->
    List.iter (fun v ->
      if v.ctor_name = "Some" || v.ctor_name = "None" then
        raise (Type_error
          (Printf.sprintf
             "%S is a built-in Option constructor and cannot be redeclared"
             v.ctor_name))) td.variants) types;
  let types = builtin_option_decl :: types in
  let env = build_env types records funcs externs in
  check_no_recursive_types env.types env.records;
  let typed_funcs = List.map (check_func env) funcs in
  let typed_externs =
    List.map (fun (e : extern_decl) ->
      let (_, (param_tys, ret_ty)) = List.assoc e.ext_name env.fns in
      { T.name = e.ext_name;
        T.params = List.combine (List.map fst e.ext_params) param_tys;
        T.return_ty = ret_ty }) externs
  in
  let resolved_types   = List.map snd env.types in
  let resolved_records = List.map snd env.records in
  (match List.find_opt (fun (f : T.func) -> f.name = "main") typed_funcs with
   | None ->
       raise (Type_error "program must define `fn main() -> int`")
   | Some f ->
       if f.T.type_params <> [] then
         raise (Type_error "`main` must not have type parameters");
       if f.T.params <> [] then
         raise (Type_error "`main` must take no parameters");
       if f.T.return_ty <> TyInt then
         raise (Type_error "`main` must return int"));
  { T.types   = resolved_types;
    T.records = resolved_records;
    T.funcs   = typed_funcs;
    T.externs = typed_externs }

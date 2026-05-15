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
                     Own[T] bindings that are not consumed. *)
    | TEMatch  of expr * ty * (pat * expr) list * ty
    | TERef    of expr * ty
                  (* ref(value) — result type is Ref[T] *)
    | TEDeref  of expr * ty
                  (* deref(ref) — result type is Option[T] *)
    | TEAssign of expr * expr * ty
                  (* (ref := value) — result type is Option[T] *)
    | TEPanic  of ty
                  (* panic() — type determined by usage; we record it
                     so emit knows what to abort to (irrelevant value) *)
    | TEOwn    of expr * ty
                  (* own(v) — result is Own[T] *)
    | TETake   of expr * ty
                  (* take(o) — result is Own[T] (same type as input) *)
    | TEUnwrap of expr * ty
                  (* unwrap(o) — result is T (copy out of heap) *)
    | TELook   of expr * ty
                  (* look(r) — result is Option[T] *)

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

let rec zonk (t : ty) : ty =
  match prune t with
  | TyInt -> TyInt
  | TyBool -> TyBool
  | TyVar n -> TyVar n
  | TyApp (n, args) -> TyApp (n, List.map zonk args)
  | TyFun (args, ret) -> TyFun (List.map zonk args, zonk ret)
  | TyMeta _ as t -> t

let rec occurs (m : meta) (t : ty) : bool =
  match prune t with
  | TyInt | TyBool | TyVar _ -> false
  | TyApp (_, args) -> List.exists (occurs m) args
  | TyFun (args, ret) ->
      List.exists (occurs m) args || occurs m ret
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
  in
  loop [] [] [] [] prog

(* ---------- type validation ---------- *)

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
      if List.mem n in_scope then begin
        if args <> [] then
          raise (Type_error
            (Printf.sprintf
               "type parameter %S cannot take type arguments" n));
        TyVar n
      end else if n = "Ref" then begin
        (* Ref is a built-in unary type constructor. *)
        if List.length args <> 1 then
          raise (Type_error
            (Printf.sprintf
               "Ref expects exactly 1 type argument, got %d"
               (List.length args)));
        TyApp ("Ref", args)
      end else if n = "Own" then begin
        (* Own is a built-in unary type constructor — exclusive ownership. *)
        if List.length args <> 1 then
          raise (Type_error
            (Printf.sprintf
               "Own expects exactly 1 type argument, got %d"
               (List.length args)));
        TyApp ("Own", args)
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

(* ---------- building environment ---------- *)

let build_env
  (types : type_decl list)
  (records : record_decl list)
  (funcs : func list)
  (externs : extern_decl list) : env =
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
          { v with arg_tys =
              List.map (validate_ty type_env record_env in_scope) v.arg_tys })
          td.variants
      in
      { td with variants }) types
  in
  let records =
    List.map (fun (rd : record_decl) ->
      let in_scope = rd.rec_type_params in
      let fields =
        List.map (fun (fname, fty) ->
          (fname, validate_ty type_env record_env in_scope fty))
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
  { types = type_env;
    records = record_env;
    ctors = ctor_env;
    fns = user_sigs @ extern_sigs }

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

(* ---------- pattern checking ---------- *)

let check_match_arms_structure
  (env : env)
  (type_name : string)
  (arms : (pat * 'a) list) : unit =
  let td = List.assoc type_name env.types in
  let all_ctors = List.map (fun v -> v.ctor_name) td.variants in
  let seen = Hashtbl.create 8 in
  let has_wildcard = ref false in
  List.iter (fun (p, _) ->
    if !has_wildcard then
      raise (Type_error "unreachable pattern after wildcard");
    match p with
    | PWild -> has_wildcard := true
    | PCtor (c, _) ->
        if Hashtbl.mem seen c then
          raise (Type_error
            (Printf.sprintf "duplicate pattern %S in match" c));
        if not (List.mem c all_ctors) then
          raise (Type_error
            (Printf.sprintf
               "constructor %S does not belong to type %S"
               c type_name));
        Hashtbl.add seen c ()) arms;
  if not !has_wildcard then begin
    let missing =
      List.filter (fun c -> not (Hashtbl.mem seen c)) all_ctors
    in
    if missing <> [] then
      raise (Type_error
        (Printf.sprintf "non-exhaustive match: missing %s"
           (String.concat ", " missing)))
  end

(* ---------- copyability ----------

   A type is copyable iff `let y = x` makes semantic sense for it —
   i.e. the value can be duplicated without violating ownership.
   Currently the only non-copyable primitive is Own[_]; structural
   types inherit non-copyability transitively from their fields/variants.
   Ref[_] is copyable (it's just pointer + generation tag).
   Type variables are assumed copyable for now — generics carry no
   bounds yet, and Own-typed arguments can't reach a generic position
   without an explicit move. *)
let rec is_copyable (env : env) (t : ty) : bool =
  match prune t with
  | TyInt | TyBool -> true
  | TyVar _        -> true
  | TyFun _        -> true
  | TyMeta _       -> true
  | TyApp ("Own", _) -> false
  | TyApp ("Ref", _) -> true
  | TyApp (n, args) when List.mem_assoc n env.records ->
      let rd = List.assoc n env.records in
      let subst = List.combine rd.rec_type_params args in
      List.for_all
        (fun (_, fty) -> is_copyable env (subst_ty subst fty))
        rd.rec_fields
  | TyApp (n, args) when List.mem_assoc n env.types ->
      let td = List.assoc n env.types in
      let subst = List.combine td.type_params args in
      List.for_all
        (fun v ->
          List.for_all
            (fun aty -> is_copyable env (subst_ty subst aty))
            v.arg_tys)
        td.variants
  | TyApp _ -> true   (* unknown name — should not occur after validate_ty *)

(* ---------- consume analysis for Own ---------- *)

(* Determines whether a name `x` is "consumed" — i.e. ownership flows
   out of the current scope through x. Three ways this can happen:

   1. `take(x)` is invoked somewhere in the expression tree.
   2. `x` is passed (as a bare EVar) into a position that requires
      moving — a function argument, a constructor argument, or a
      record field value — when the type at that position is
      non-copyable. The "position is non-copyable" test uses the
      type of x itself (which after unification must match the
      formal type), so this naturally subsumes both `Own[T]` and
      any struct/ADT containing Own.
   3. `x` appears in *tail position* — as the final result of some
      branch of computation, meaning ownership flows out of the let
      that bound it.

   Reads that do not move ownership are NOT consumes: `unwrap(x)`,
   `deref(x)`, `look(x)`, `x.field`. `ref(x)` is the legacy alloc
   form; in the new model it would be a borrow that doesn't consume,
   so we deliberately don't count it here. *)

(* Used by takes_consume to recognise a bare consume in arg position. *)
let consumed_in_arg (env : env) (x : string) (a : T.expr) : bool =
  match a with
  | T.TEVar (y, t) when y = x -> not (is_copyable env t)
  | _ -> false

(* takes_consume: does the expression contain a direct consume of `x`? *)
let rec takes_consume (env : env) (x : string) (e : T.expr) : bool =
  match e with
  | T.TEInt _ | T.TEBool _ | T.TEVar _ | T.TEFnRef _ -> false
  | T.TECall (callee, args, _) ->
      takes_consume env x callee
      || List.exists (takes_consume env x) args
      || List.exists (consumed_in_arg env x) args
  | T.TEBinop (_, a, b, _) ->
      takes_consume env x a || takes_consume env x b
  | T.TEUnop (_, a, _) -> takes_consume env x a
  | T.TECtor (_, _, args, _) ->
      List.exists (takes_consume env x) args
      || List.exists (consumed_in_arg env x) args
  | T.TERecord (_, _, fields, _) ->
      List.exists (fun (_, e) -> takes_consume env x e) fields
      || List.exists (fun (_, e) -> consumed_in_arg env x e) fields
  | T.TEField (e, _, _) -> takes_consume env x e
  | T.TEIf (c, t, el, _) ->
      takes_consume env x c
      || takes_consume env x t
      || takes_consume env x el
  | T.TELet (y, _, v, b, _, _) ->
      takes_consume env x v
      || (y <> x && takes_consume env x b)
  | T.TEMatch (s, _, arms, _) ->
      takes_consume env x s
      || List.exists (fun (p, body) ->
        let shadowed = match p with
          | PWild -> false
          | PCtor (_, names) -> List.mem x names
        in
        not shadowed && takes_consume env x body) arms
  | T.TERef (e, _) -> takes_consume env x e
  | T.TEDeref (e, _) -> takes_consume env x e
  | T.TEAssign (r, v, _) -> takes_consume env x r || takes_consume env x v
  | T.TEPanic _ -> false
  | T.TEOwn (e, _) -> takes_consume env x e
  | T.TETake (arg, _) ->
      (match arg with
       | T.TEVar (y, _) when y = x -> true
       | _ -> takes_consume env x arg)
  | T.TEUnwrap (e, _) -> takes_consume env x e
  | T.TELook (e, _) -> takes_consume env x e

(* tail_consume: does x reach the tail position of the expression? *)
let rec tail_consume (x : string) (e : T.expr) : bool =
  match e with
  | T.TEVar (y, _) -> y = x
  | T.TELet (y, _, _, body, _, _) ->
      y <> x && tail_consume x body
  | T.TEIf (_, t, el, _) -> tail_consume x t || tail_consume x el
  | T.TEMatch (_, _, arms, _) ->
      List.exists (fun (p, body) ->
        let shadowed = match p with
          | PWild -> false
          | PCtor (_, names) -> List.mem x names
        in
        not shadowed && tail_consume x body) arms
  | _ -> false   (* any other terminal: int, ctor, call result, ... — not x *)

let is_consumed (env : env) (x : string) (e : T.expr) : bool =
  takes_consume env x e || tail_consume x e

let rec infer (env : env) (tparams : string list)
  (vars : (string * ty) list) (e : expr)
  : T.expr * ty =
  match e with
  | EInt n  -> (T.TEInt n,  TyInt)
  | EBool b -> (T.TEBool b, TyBool)

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
             | TyMeta _ -> unify ta_ty TyInt   (* default to int *)
             | t ->
                 raise (Type_error
                   (Printf.sprintf
                      "%s requires int or bool operands, got %s"
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
       | Some t -> (T.TEVar (x, t), t)
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

  | ELet (x, ascription, value, body) ->
      if x <> "_" then check_not_c_reserved "let-binding" x;
      (* Non-copyable bindings cannot be aliased through `let y = x`.
         The legal moves are explicit: `take(x)` to transfer ownership,
         `ref(x)` to borrow. Detect the bare-EVar form here, before
         inference, so the error names the source identifier. *)
      (match value with
       | EVar y ->
           (match List.assoc_opt y vars with
            | Some yty when not (is_copyable env yty) ->
                raise (Type_error
                  (Printf.sprintf
                     "cannot copy %S: type %s is not copyable; \
                      use take(%s) to move ownership or ref(%s) to borrow"
                     y (show_ty (zonk yty)) y y))
            | _ -> ())
       | _ -> ());
      let (tv, tv_ty) = infer env tparams vars value in
      (match ascription with
       | None -> ()
       | Some t ->
           let t = validate_ty_for_ascription env tparams t in
           unify tv_ty t);
      let body_vars = if x = "_" then vars else (x, tv_ty) :: vars in
      let (tb, tb_ty) = infer env tparams body_vars body in
      (* auto_drop applies when:
           - the bound type is Own[T]
           - x is a real name (not "_")
           - x is NOT consumed in the body *)
      let auto_drop =
        if x = "_" then false
        else
          (match prune tv_ty with
           | TyApp ("Own", _) -> not (is_consumed env x tb)
           | _ -> false)
      in
      (T.TELet (x, tv_ty, tv, tb, tb_ty, auto_drop), tb_ty)

  | EMatch (scrut, arms) ->
      if arms = [] then
        raise (Type_error "match must have at least one arm");
      let (tscrut, tscrut_ty) = infer env tparams vars scrut in
      let scrut_ty_now = prune tscrut_ty in
      let type_name =
        match scrut_ty_now with
        | TyApp (n, _) when List.mem_assoc n env.types -> n
        | _ ->
            raise (Type_error
              (Printf.sprintf
                 "match scrutinee must have an ADT type, got %s"
                 (show_ty (zonk tscrut_ty))))
      in
      check_match_arms_structure env type_name arms;
      let typed_arms = List.map (fun (pat, body) ->
        let body_vars =
          match pat with
          | PWild -> vars
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
                if v = "_" then acc else (v, t) :: acc)
                vars vs arg_tys
        in
        let (tbody, tbody_ty) = infer env tparams body_vars body in
        ((pat, tbody), tbody_ty)) arms
      in
      let first_ty = snd (List.hd typed_arms) in
      List.iter (fun (_, t) -> unify first_ty t) typed_arms;
      let arms_out = List.map fst typed_arms in
      (T.TEMatch (tscrut, tscrut_ty, arms_out, first_ty), first_ty)

  | ERef value ->
      let (tv, tv_ty) = infer env tparams vars value in
      let result_ty = TyApp ("Ref", [tv_ty]) in
      (T.TERef (tv, result_ty), result_ty)

  | EDeref r ->
      let (tr, tr_ty) = infer env tparams vars r in
      let inner = TyMeta (fresh_meta ()) in
      let ref_ty = TyApp ("Ref", [inner]) in
      (try unify ref_ty tr_ty
       with Type_error _ ->
         raise (Type_error
           (Printf.sprintf
              "deref expects a Ref[T], got %s"
              (show_ty (zonk tr_ty)))));
      let result_ty = TyApp ("Option", [inner]) in
      (T.TEDeref (tr, result_ty), result_ty)

  | EAssign (r, v) ->
      let (tr, tr_ty) = infer env tparams vars r in
      let inner = TyMeta (fresh_meta ()) in
      let ref_ty = TyApp ("Ref", [inner]) in
      (try unify ref_ty tr_ty
       with Type_error _ ->
         raise (Type_error
           (Printf.sprintf
              "`:=` expects a Ref[T] on the left, got %s"
              (show_ty (zonk tr_ty)))));
      let (tv, tv_ty) = infer env tparams vars v in
      (try unify inner tv_ty
       with Type_error _ ->
         raise (Type_error
           (Printf.sprintf
              "type mismatch in `:=`: ref holds %s, value is %s"
              (show_ty (zonk inner)) (show_ty (zonk tv_ty)))));
      let result_ty = TyApp ("Option", [inner]) in
      (T.TEAssign (tr, tv, result_ty), result_ty)

  | EOrElse (a, b) ->
      (* Desugar `a ?? b` into `match a { Some(_qq_v) => _qq_v, None => b }`. *)
      let inner = TyMeta (fresh_meta ()) in
      let opt_ty = TyApp ("Option", [inner]) in
      let (ta, ta_ty) = infer env tparams vars a in
      (try unify opt_ty ta_ty
       with Type_error _ ->
         raise (Type_error
           (Printf.sprintf
              "left of `??` must be an Option[T], got %s"
              (show_ty (zonk ta_ty)))));
      let (tb, tb_ty) = infer env tparams vars b in
      (try unify inner tb_ty
       with Type_error _ ->
         raise (Type_error
           (Printf.sprintf
              "right of `??` must be %s, got %s"
              (show_ty (zonk inner)) (show_ty (zonk tb_ty)))));
      let some_body = T.TEVar ("_qq_v", inner) in
      let arms : (pat * T.expr) list = [
        (PCtor ("Some", ["_qq_v"]), some_body);
        (PCtor ("None", []), tb);
      ] in
      (T.TEMatch (ta, ta_ty, arms, inner), inner)

  | EPanic ->
      let result_ty = TyMeta (fresh_meta ()) in
      (T.TEPanic result_ty, result_ty)

  | EOwn value ->
      (* own(v) : T → Own[T] — allocate cell on heap, give exclusive ownership *)
      let (tv, tv_ty) = infer env tparams vars value in
      let result_ty = TyApp ("Own", [tv_ty]) in
      (T.TEOwn (tv, result_ty), result_ty)

  | ETake o ->
      (* take(o) : Own[T] → Own[T] — explicit consume. Type unchanged.
         Consume tracking happens later (next sub-stage); for now we just
         check the type is Own[T]. *)
      let (to_e, to_ty) = infer env tparams vars o in
      let inner = TyMeta (fresh_meta ()) in
      let own_ty = TyApp ("Own", [inner]) in
      (try unify own_ty to_ty
       with Type_error _ ->
         raise (Type_error
           (Printf.sprintf
              "take expects an Own[T], got %s"
              (show_ty (zonk to_ty)))));
      (T.TETake (to_e, own_ty), own_ty)

  | EUnwrap o ->
      (* unwrap(o) : Own[T] → T — copy value out of heap.
         Restriction: T must be copyable. We don't enforce this in this
         sub-stage yet — that's part of the copyability analysis to come. *)
      let (to_e, to_ty) = infer env tparams vars o in
      let inner = TyMeta (fresh_meta ()) in
      let own_ty = TyApp ("Own", [inner]) in
      (try unify own_ty to_ty
       with Type_error _ ->
         raise (Type_error
           (Printf.sprintf
              "unwrap expects an Own[T], got %s"
              (show_ty (zonk to_ty)))));
      (T.TEUnwrap (to_e, inner), inner)

  | ELook r ->
      (* look(r) : Ref[T] → Option[T] — read through observer, may fail.
         Restriction: T must be copyable. Not enforced here yet. *)
      let (tr, tr_ty) = infer env tparams vars r in
      let inner = TyMeta (fresh_meta ()) in
      let ref_ty = TyApp ("Ref", [inner]) in
      (try unify ref_ty tr_ty
       with Type_error _ ->
         raise (Type_error
           (Printf.sprintf
              "look expects a Ref[T], got %s"
              (show_ty (zonk tr_ty)))));
      let result_ty = TyApp ("Option", [inner]) in
      (T.TELook (tr, result_ty), result_ty)

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
  match t with
  | TyInt | TyBool -> t
  | TyVar _ -> t
  | TyMeta _ -> t
  | TyApp (n, args) ->
      let args = List.map (validate_ty_for_ascription env tparams) args in
      if List.mem n tparams then begin
        if args <> [] then
          raise (Type_error
            (Printf.sprintf
               "type parameter %S cannot take type arguments" n));
        TyVar n
      end else if n = "Ref" then begin
        if List.length args <> 1 then
          raise (Type_error
            (Printf.sprintf
               "Ref expects exactly 1 type argument, got %d"
               (List.length args)));
        TyApp ("Ref", args)
      end else if n = "Own" then begin
        if List.length args <> 1 then
          raise (Type_error
            (Printf.sprintf
               "Own expects exactly 1 type argument, got %d"
               (List.length args)));
        TyApp ("Own", args)
      end else
        (match List.assoc_opt n env.types with
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
             (match List.assoc_opt n env.records with
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
      TyFun (List.map (validate_ty_for_ascription env tparams) args,
             validate_ty_for_ascription env tparams ret)

(* ---------- zonk the typed AST ---------- *)

let rec zonk_expr (e : T.expr) : T.expr =
  match e with
  | T.TEInt _ | T.TEBool _ -> e
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
  | T.TERef (e, t) ->
      T.TERef (zonk_expr e, zonk_expect t)
  | T.TEDeref (e, t) ->
      T.TEDeref (zonk_expr e, zonk_expect t)
  | T.TEAssign (r, v, t) ->
      T.TEAssign (zonk_expr r, zonk_expr v, zonk_expect t)
  | T.TEPanic t ->
      T.TEPanic (zonk_expect t)
  | T.TEOwn (e, t) ->
      T.TEOwn (zonk_expr e, zonk_expect t)
  | T.TETake (e, t) ->
      T.TETake (zonk_expr e, zonk_expect t)
  | T.TEUnwrap (e, t) ->
      T.TEUnwrap (zonk_expr e, zonk_expect t)
  | T.TELook (e, t) ->
      T.TELook (zonk_expr e, zonk_expect t)

and zonk_expect (t : ty) : ty =
  let t = zonk t in
  let rec has_unresolved = function
    | TyInt | TyBool | TyVar _ -> false
    | TyApp (_, args) -> List.exists has_unresolved args
    | TyFun (args, ret) ->
        List.exists has_unresolved args || has_unresolved ret
    | TyMeta _ -> true
  in
  if has_unresolved t then
    raise (Type_error
      (Printf.sprintf
         "could not infer type (still %s after checking) — \
          consider adding a `let x: SomeType = ...` annotation"
         (show_ty t)))
  else t

(* ---------- check a function ---------- *)

let check_func (env : env) (f : func) : T.func =
  let (_, (param_tys, ret_ty)) = List.assoc f.name env.fns in
  let vars = List.combine (List.map fst f.params) param_tys in
  let tparams = f.type_params in
  let (tbody, tbody_ty) = infer env tparams vars f.body in
  (try unify ret_ty tbody_ty
   with Type_error _ ->
     raise (Type_error
       (Printf.sprintf
          "function %S: body has type %s, declared return type is %s"
          f.name (show_ty (zonk tbody_ty)) (show_ty (zonk ret_ty)))));
  let tbody = zonk_expr tbody in
  (* A parameter of type Own[T] has its scope = the whole body. If the
     body doesn't move ownership out (take, tail return, or pass to
     another consuming position), the cell must be freed before the
     function returns. We express this by wrapping the body in a chain
     of `let p = p; body` bindings — the outer TELet's auto_drop flag
     reuses the same drop machinery as ordinary let-bindings. Done
     fold_right so the first parameter ends up outermost, giving LIFO
     drop order relative to the parameter list. Alpha-rename later
     gives the inner `p` a fresh name to avoid C variable collisions. *)
  (* A parameter of type Own[T] has its scope = the whole body. If the
     body doesn't move ownership out (take, tail return, or pass to
     another consuming position), the cell must be freed before the
     function returns. We express this by wrapping the body in a chain
     of `let p = p; body` bindings — the outer TELet's auto_drop flag
     reuses the same drop machinery as ordinary let-bindings. Done
     fold_right so the first parameter ends up outermost, giving LIFO
     drop order relative to the parameter list. Alpha-rename later
     gives the inner `p` a fresh name to avoid C variable collisions. *)
  let tbody_ty = zonk tbody_ty in
  let param_tys = List.map zonk param_tys in
  let body_with_drops =
    List.fold_right (fun (pname, pty) acc ->
      match prune pty with
      | TyApp ("Own", _) when not (is_consumed env pname acc) ->
          T.TELet (pname, pty, T.TEVar (pname, pty),
                   acc, tbody_ty, true)
      | _ -> acc)
      (List.combine (List.map fst f.params) param_tys)
      tbody
  in
  { T.name = f.name;
    T.type_params = f.type_params;
    T.params = List.combine
      (List.map fst f.params) param_tys;
    T.return_ty = ret_ty;
    T.body = body_with_drops }

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
}

let check (prog : program) : T.program =
  meta_counter := 0;
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

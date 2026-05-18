(* Module resolution.

   Input:  list of parsed modules, each (module_name, program).
   Output: single combined program with all `use` decls removed and
           every cross-module reference rewritten to its mangled form.

   Mangling: a top-level decl named `foo` in module `bar` becomes
   `bar__foo`. Externs are NOT mangled — their name is the C linker
   symbol and must round-trip unchanged. Builtins (Array, Region,
   Option, byte, Some, None) are never mangled.

   Within a module, references resolve in this order:
     1. Local scope (let bindings, parameters, pattern vars, type
        parameters) — never renamed.
     2. The module's `use foo::bar;` imports — renamed to `foo__bar`
        (or kept bare if `bar` is an extern in `foo`).
     3. The module's own top-level decls — renamed to `M__name`.
     4. Builtins — kept as-is.
     5. Anything else — left alone (check.ml will complain). *)

open Ast

exception Resolve_error of string

let builtin_names = [
  "Array"; "Region"; "Option"; "byte";
  "Some"; "None";
]

let is_builtin name = List.mem name builtin_names

(* `main` is the C entry point — every program has exactly one and it
   keeps its bare name. Any module can declare it, but only one in the
   whole compilation may. *)
let is_unmanglable name = (name = "main")

let mangle_for_module (module_name : string) (name : string) : string =
  if is_unmanglable name then name
  else module_name ^ "__" ^ name

(* What a single module exports, as a set of (bare_name, mangled_name)
   pairs. Externs use bare_name = mangled_name (no mangling). *)
type module_summary = {
  mod_name      : string;
  type_names    : (string * string) list;
  record_names  : (string * string) list;
  fn_names      : (string * string) list;
  ctor_names    : (string * string) list;
  alias_names   : (string * string) list;
  extern_names  : string list;
}

let summarize (module_name : string) (prog : program) : module_summary =
  let m n = mangle_for_module module_name n in
  let types   = ref [] in
  let records = ref [] in
  let fns     = ref [] in
  let ctors   = ref [] in
  let aliases = ref [] in
  let externs = ref [] in
  List.iter (fun decl ->
    match decl with
    | TopType td ->
        types := (td.type_name, m td.type_name) :: !types;
        List.iter (fun v ->
          ctors := (v.ctor_name, m v.ctor_name) :: !ctors) td.variants
    | TopRecord rd ->
        records := (rd.rec_name, m rd.rec_name) :: !records
    | TopFunc f ->
        fns := (f.name, m f.name) :: !fns
    | TopExtern e ->
        externs := e.ext_name :: !externs
    | TopAlias a ->
        aliases := (a.alias_name, m a.alias_name) :: !aliases
    | TopUse _ -> ()) prog;
  { mod_name = module_name;
    type_names   = !types;
    record_names = !records;
    fn_names     = !fns;
    ctor_names   = !ctors;
    alias_names  = !aliases;
    extern_names = !externs }

(* For a module M, build the resolution table that maps bare names
   visible in M's source to their target form (mangled or unchanged). *)
let build_resolution_map
    (m_summary : module_summary)
    (uses : use_decl list)
    (all_summaries : module_summary list)
  : (string * string) list =
  let own =
    m_summary.type_names
    @ m_summary.record_names
    @ m_summary.fn_names
    @ m_summary.ctor_names
    @ m_summary.alias_names
    @ List.map (fun e -> (e, e)) m_summary.extern_names
  in
  let imports =
    List.concat_map (fun u ->
      let other =
        try List.find (fun s -> s.mod_name = u.use_module) all_summaries
        with Not_found ->
          raise (Resolve_error
            (Printf.sprintf "module %S not found (referenced by `use`)"
               u.use_module))
      in
      List.map (fun item ->
        let lookup name lst = List.assoc_opt name lst in
        match lookup item other.type_names with
        | Some m -> (item, m)
        | None ->
        match lookup item other.record_names with
        | Some m -> (item, m)
        | None ->
        match lookup item other.fn_names with
        | Some m -> (item, m)
        | None ->
        match lookup item other.ctor_names with
        | Some m -> (item, m)
        | None ->
        match lookup item other.alias_names with
        | Some m -> (item, m)
        | None ->
          if List.mem item other.extern_names then (item, item)
          else raise (Resolve_error
            (Printf.sprintf
               "use %s::%s — %S is not declared in module %S"
               u.use_module item item u.use_module))) u.use_items)
      uses
  in
  let bare_own = List.map fst own in
  List.iter (fun (n, _) ->
    if List.mem n bare_own then
      raise (Resolve_error
        (Printf.sprintf
           "imported name %S clashes with a local declaration in module %S"
           n m_summary.mod_name))) imports;
  let rec check_dups = function
    | [] -> ()
    | (x, _) :: rest ->
        if List.exists (fun (y, _) -> y = x) rest then
          raise (Resolve_error
            (Printf.sprintf "duplicate import of %S in module %S"
               x m_summary.mod_name));
        check_dups rest
  in
  check_dups imports;
  own @ imports

let resolve_name
    (map : (string * string) list) (locals : string list) (name : string)
  : string =
  if List.mem name locals then name
  else if is_builtin name then name
  else
    match List.assoc_opt name map with
    | Some renamed -> renamed
    | None -> name

let rec resolve_ty
    (map : (string * string) list) (locals : string list) (t : ty) : ty =
  match t with
  | TyInt | TyBool -> t
  | TyVar n -> TyVar n
  | TyApp (n, args) ->
      let args = List.map (resolve_ty map locals) args in
      TyApp (resolve_name map locals n, args)
  | TyFun (args, ret) ->
      TyFun (List.map (resolve_ty map locals) args,
             resolve_ty map locals ret)
  | TyPtr inner -> TyPtr (resolve_ty map locals inner)
  | TyMeta _ -> t

let rec resolve_expr
    (map : (string * string) list) (locals : string list) (e : expr) : expr =
  let r = resolve_expr map locals in
  let rt = resolve_ty map locals in
  match e with
  | EInt _ | EBool _ | EStringLit _ -> e
  | EVar x -> EVar (resolve_name map locals x)
  | EBinop (op, a, b) -> EBinop (op, r a, r b)
  | EUnop  (op, a)    -> EUnop  (op, r a)
  | ECall (callee, args) -> ECall (r callee, List.map r args)
  | ECtor (c, args) ->
      ECtor (resolve_name map locals c, List.map r args)
  | ERecord (name, elems) ->
      let elems = List.map (function
        | RAssign (f, v) -> RAssign (f, r v)
        | RSpread b -> RSpread (r b)) elems
      in
      ERecord (resolve_name map locals name, elems)
  | EField (e, f) -> EField (r e, f)
  | EIf (c, t, el) -> EIf (r c, r t, r el)
  | ELet (x, m, asc, v, body) ->
      let v' = r v in
      let asc' = Option.map rt asc in
      let new_locals = if x = "_" then locals else x :: locals in
      let body' = resolve_expr map new_locals body in
      ELet (x, m, asc', v', body')
  | EAssign (x, v) -> EAssign (resolve_name map locals x, r v)
  | EWhile (c, b) -> EWhile (r c, r b)
  | EBreak | EContinue -> e
  | EReturn v -> EReturn (r v)
  | EMatch (s, arms) ->
      let s' = r s in
      let rec resolve_pat p =
        match p with
        | PCtor (c, vs) -> PCtor (resolve_name map locals c, vs)
        | POr pats -> POr (List.map resolve_pat pats)
        | PInt _ | PBool _ | PStr _ | PBind _ -> p
      in
      let pattern_locals p =
        match p with
        | PCtor (_, vs) -> List.filter (fun v -> v <> "_") vs
        | POr _ -> []
        | PInt _ | PBool _ | PStr _ -> []
        | PBind "_" -> []
        | PBind x -> [x]
      in
      let arms' = List.map (fun (p, guard, body) ->
        let p' = resolve_pat p in
        let new_locals = pattern_locals p @ locals in
        let guard' = Option.map (resolve_expr map new_locals) guard in
        (p', guard', resolve_expr map new_locals body)) arms
      in
      EMatch (s', arms')
  | EArray (rg, n, v) -> EArray (r rg, r n, r v)
  | EArrayLit (rg, elems) -> EArrayLit (r rg, List.map r elems)
  | ERegion n -> ERegion (r n)
  | EStackRegion n -> EStackRegion (r n)
  | EAlignedRegion (n, a) -> EAlignedRegion (r n, r a)
  | EIndex (a, i) -> EIndex (r a, r i)
  | EAssignIdx (a, i, v) -> EAssignIdx (r a, r i, r v)
  | ELen e -> ELen (r e)
  | ESlice (a, lo, hi) -> ESlice (r a, r lo, r hi)
  | EToInt e -> EToInt (r e)
  | EToByte e -> EToByte (r e)
  | ECAlloc (t, n) -> ECAlloc (rt t, r n)
  | ECFree p -> ECFree (r p)
  | ENullPtr t -> ENullPtr (rt t)
  | EIsNull p -> EIsNull (r p)
  | EArrayData a -> EArrayData (r a)
  | EDeref p -> EDeref (r p)
  | ETryAt (a, i) -> ETryAt (r a, r i)
  | EDrop e -> EDrop (r e)

let resolve_decl
    (map : (string * string) list) (mod_name : string) (decl : top_decl)
  : top_decl option =
  let m_name n = mangle_for_module mod_name n in
  match decl with
  | TopUse _ -> None
  | TopAlias a ->
      Some (TopAlias {
        alias_name = m_name a.alias_name;
        alias_ty   = resolve_ty map [] a.alias_ty;
      })
  | TopType td ->
      let type_params = td.type_params in
      let locals = type_params in
      let variants = List.map (fun v ->
        { ctor_name = m_name v.ctor_name;
          arg_tys = List.map (resolve_ty map locals) v.arg_tys })
        td.variants
      in
      Some (TopType {
        type_name = m_name td.type_name;
        type_params; variants;
        is_linear = td.is_linear;
      })
  | TopRecord rd ->
      let type_params = rd.rec_type_params in
      let locals = type_params in
      let rec_fields = List.map (fun (fn, ft) ->
        (fn, resolve_ty map locals ft)) rd.rec_fields
      in
      Some (TopRecord {
        rec_name = m_name rd.rec_name;
        rec_type_params = type_params;
        rec_fields;
        rec_is_linear = rd.rec_is_linear;
      })
  | TopFunc f ->
      let type_params = f.type_params in
      let locals = type_params in
      let params = List.map (fun (pn, pt) ->
        (pn, resolve_ty map locals pt)) f.params
      in
      let return_ty = resolve_ty map locals f.return_ty in
      let param_locals = List.map fst params in
      let body_locals = param_locals @ type_params in
      let body = resolve_expr map body_locals f.body in
      Some (TopFunc {
        name = m_name f.name;
        type_params; params; return_ty; body;
      })
  | TopExtern e ->
      let params = List.map (fun (pn, pt) ->
        (pn, resolve_ty map [] pt)) e.ext_params
      in
      let return_ty = resolve_ty map [] e.ext_return_ty in
      Some (TopExtern {
        ext_name = e.ext_name;
        ext_params = params;
        ext_return_ty = return_ty;
      })

(* Expand type aliases transitively, with cycle detection. *)
let rec expand_alias_ty
    (aliases : (string * ty) list) (visited : string list) (t : ty) : ty =
  match t with
  | TyInt | TyBool -> t
  | TyVar _ -> t
  | TyMeta _ -> t
  | TyApp (n, args) ->
      let args = List.map (expand_alias_ty aliases visited) args in
      (match List.assoc_opt n aliases with
       | Some target ->
           if List.mem n visited then
             raise (Resolve_error
               (Printf.sprintf "cyclic type alias: %s"
                  (String.concat " -> " (List.rev (n :: visited)))));
           if args <> [] then
             raise (Resolve_error
               (Printf.sprintf
                  "type alias %S cannot take type arguments" n));
           expand_alias_ty aliases (n :: visited) target
       | None -> TyApp (n, args))
  | TyFun (args, ret) ->
      TyFun (List.map (expand_alias_ty aliases visited) args,
             expand_alias_ty aliases visited ret)
  | TyPtr inner -> TyPtr (expand_alias_ty aliases visited inner)

let expand_in_expr (aliases : (string * ty) list) (e : expr) : expr =
  let xt t = expand_alias_ty aliases [] t in
  let rec ex e =
    match e with
    | EInt _ | EBool _ | EStringLit _ | EVar _
    | EBreak | EContinue -> e
    | EBinop (op, a, b) -> EBinop (op, ex a, ex b)
    | EUnop (op, a) -> EUnop (op, ex a)
    | ECall (c, args) -> ECall (ex c, List.map ex args)
    | ECtor (c, args) -> ECtor (c, List.map ex args)
    | ERecord (n, elems) ->
        ERecord (n, List.map (function
          | RAssign (f, v) -> RAssign (f, ex v)
          | RSpread b -> RSpread (ex b)) elems)
    | EField (e, f) -> EField (ex e, f)
    | EIf (c, t, e) -> EIf (ex c, ex t, ex e)
    | ELet (x, m, asc, v, b) -> ELet (x, m, Option.map xt asc, ex v, ex b)
    | EAssign (x, v) -> EAssign (x, ex v)
    | EWhile (c, b) -> EWhile (ex c, ex b)
    | EReturn v -> EReturn (ex v)
    | EMatch (s, arms) ->
        EMatch (ex s, List.map (fun (p, g, b) ->
          (p, Option.map ex g, ex b)) arms)
    | EArray (r, n, v) -> EArray (ex r, ex n, ex v)
    | EArrayLit (r, elems) -> EArrayLit (ex r, List.map ex elems)
    | ERegion n -> ERegion (ex n)
    | EStackRegion n -> EStackRegion (ex n)
    | EAlignedRegion (n, a) -> EAlignedRegion (ex n, ex a)
    | EIndex (a, i) -> EIndex (ex a, ex i)
    | EAssignIdx (a, i, v) -> EAssignIdx (ex a, ex i, ex v)
    | ELen e -> ELen (ex e)
    | ESlice (a, lo, hi) -> ESlice (ex a, ex lo, ex hi)
    | EToInt e -> EToInt (ex e)
    | EToByte e -> EToByte (ex e)
    | ECAlloc (t, n) -> ECAlloc (xt t, ex n)
    | ECFree p -> ECFree (ex p)
    | ENullPtr t -> ENullPtr (xt t)
    | EIsNull p -> EIsNull (ex p)
    | EArrayData a -> EArrayData (ex a)
    | EDeref p -> EDeref (ex p)
    | ETryAt (a, i) -> ETryAt (ex a, ex i)
    | EDrop e -> EDrop (ex e)
  in
  ex e

let expand_in_decl (aliases : (string * ty) list) (d : top_decl) : top_decl =
  let xt t = expand_alias_ty aliases [] t in
  match d with
  | TopType td ->
      TopType { td with variants =
        List.map (fun v ->
          { v with arg_tys = List.map xt v.arg_tys }) td.variants;
        is_linear = td.is_linear; }
  | TopRecord rd ->
      TopRecord { rd with rec_fields =
        List.map (fun (f, t) -> (f, xt t)) rd.rec_fields;
        rec_is_linear = rd.rec_is_linear; }
  | TopFunc f ->
      TopFunc { f with
        params = List.map (fun (n, t) -> (n, xt t)) f.params;
        return_ty = xt f.return_ty;
        body = expand_in_expr aliases f.body; }
  | TopExtern e ->
      TopExtern { e with
        ext_params = List.map (fun (n, t) -> (n, xt t)) e.ext_params;
        ext_return_ty = xt e.ext_return_ty; }
  | TopUse _ | TopAlias _ -> d

(* Top-level entry: take an ordered list of (module_name, parsed program),
   resolve all references, return one merged program ready for the type
   checker. Externs are deduplicated by name. *)
let resolve (modules : (string * program) list) : program =
  let summaries = List.map (fun (mn, p) -> summarize mn p) modules in
  let resolved_per_module = List.map (fun (mn, prog) ->
    let m_summary = List.find (fun s -> s.mod_name = mn) summaries in
    let uses = List.filter_map (function
      | TopUse u -> Some u
      | _ -> None) prog
    in
    let map = build_resolution_map m_summary uses summaries in
    List.filter_map (resolve_decl map mn) prog
  ) modules
  in
  let combined = List.concat resolved_per_module in
  (* Collect aliases by their mangled name, then expand all references
     to them throughout the program. *)
  let aliases =
    List.filter_map (function
      | TopAlias a -> Some (a.alias_name, a.alias_ty)
      | _ -> None) combined
  in
  let expanded = List.map (expand_in_decl aliases) combined in
  let no_aliases = List.filter (function
    | TopAlias _ -> false | _ -> true) expanded
  in
  (* Deduplicate externs by name — multiple modules can declare the
     same C function (e.g. putchar). First occurrence wins; later
     duplicates with different signatures are caught later by check. *)
  let seen_externs = Hashtbl.create 8 in
  List.filter (function
    | TopExtern e ->
        if Hashtbl.mem seen_externs e.ext_name then false
        else (Hashtbl.add seen_externs e.ext_name (); true)
    | _ -> true) no_aliases

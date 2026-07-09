(* Module resolution.

   Input:  list of parsed modules, each (file_name, program).
   Output: single combined program with all `use` and `namespace`
           decls removed and every cross-namespace reference rewritten
           to its mangled form.

   Namespaces: a file `foo.orto` implicitly defines a namespace `foo`
   for decls outside any `namespace { ... }` block. Each
   `namespace a::b::c { decls }` block defines that dotted namespace.
   Multiple files may declare the same namespace; they all merge.

   Mangling: decl `foo` in namespace `a::b` becomes `a__b__foo`.
   Externs are NOT mangled — their name is the C linker symbol and
   must round-trip unchanged. Builtins (Handle, Region, Option, byte,
   Some, None, Result, Ok, Err) are never mangled.

   Within a namespace, references resolve in this order:
     1. Local scope (let bindings, parameters, pattern vars, type
        parameters) — never renamed.
     2. The namespace's `use a::b::{x};` imports — renamed to
        `a__b__x` (or kept bare if `x` is an extern in `a::b`).
     3. The namespace's own top-level decls — renamed with its
        mangled prefix.
     4. Builtins — kept as-is.
     5. Anything else — left alone (check.ml will complain). *)

open Ast

exception Resolve_error of string

let builtin_names = [
  "Handle"; "Region"; "Option"; "byte";
  "Some"; "None";
  "Result"; "Ok"; "Err";
]

let is_builtin name = List.mem name builtin_names

(* Mangled names of all `const` declarations across modules. A const is
   lowered to a 0-argument function; a *use* of a const name must become
   a call. Populated in `summarize`, read in `resolve_expr`. *)
let const_set : (string, unit) Hashtbl.t = Hashtbl.create 16

(* `main` is the C entry point — every program has exactly one and it
   keeps its bare name. Any module can declare it, but only one in the
   whole compilation may. *)
let is_unmanglable name = (name = "main")

let path_to_string (p : string list) = String.concat "::" p
let path_to_mangle (p : string list) = String.concat "__" p

let mangle_for_module (module_path : string list) (name : string) : string =
  if is_unmanglable name then name
  else path_to_mangle module_path ^ "__" ^ name

(* What a single namespace exports, as a set of (bare_name, mangled_name)
   pairs. Externs use bare_name = mangled_name (no mangling).

   `re_exports` is filled in a second pass from `pub use mod::{name};`
   statements: those names ARE in this namespace's public surface but
   their definition lives elsewhere — bare_name → other-namespace's
   mangled name. *)
type module_summary = {
  mod_path      : string list;
  type_names    : (string * string) list;
  record_names  : (string * string) list;
  fn_names      : (string * string) list;
  ctor_names    : (string * string) list;
  alias_names   : (string * string) list;
  extern_names  : string list;
  mutable re_exports : (string * string) list;
}

(* Flatten one file's program into (namespace_path, decls) pairs.
   The default namespace = [file_name] catches decls outside any
   `namespace { ... }` block; explicit blocks contribute their own
   paths. Nested namespace blocks are not supported syntactically
   (parse_namespace_path consumes the whole dotted path inline);
   if one appears anyway we still flatten it conservatively. *)
let flatten_namespaces (file_name : string) (prog : program)
  : (string list * top_decl list) list =
  let buckets : (string list, top_decl list ref) Hashtbl.t = Hashtbl.create 4 in
  let order : string list list ref = ref [] in
  let bucket_for path =
    match Hashtbl.find_opt buckets path with
    | Some r -> r
    | None ->
        let r = ref [] in
        Hashtbl.add buckets path r;
        order := path :: !order;
        r
  in
  let rec visit path decls =
    List.iter (fun d ->
      match d with
      | TopNamespace (sub, inner) -> visit sub inner
      | _ ->
          let r = bucket_for path in
          r := d :: !r) decls
  in
  visit [file_name] prog;
  List.rev_map (fun path ->
    let r = Hashtbl.find buckets path in
    (path, List.rev !r)) !order

let summarize (module_path : string list) (prog : top_decl list) : module_summary =
  let m n = mangle_for_module module_path n in
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
    | TopConst c ->
        (* A const resolves like a (0-arg) function name, and its mangled
           name is recorded so uses get rewritten to calls. *)
        fns := (c.const_name, m c.const_name) :: !fns;
        Hashtbl.replace const_set (m c.const_name) ()
    | TopTest _ -> ()  (* test blocks don't introduce namespace-level names *)
    | TopUse _ -> ()
    | TopNamespace _ -> ()  (* flattened away by flatten_namespaces *)
  ) prog;
  { mod_path     = module_path;
    type_names   = !types;
    record_names = !records;
    fn_names     = !fns;
    ctor_names   = !ctors;
    alias_names  = !aliases;
    extern_names = !externs;
    re_exports   = [] }

(* Second pass: fill in re_exports for every namespace from its
   `pub use foo::{bar};` decls. Has to come after every namespace's
   bare summary is built, because a re-export needs to look the
   item up in the source namespace's summary. *)
let fill_re_exports
    (summaries : module_summary list)
    (per_ns : (string list * top_decl list) list) : unit =
  let find_summary p =
    try List.find (fun s -> s.mod_path = p) summaries
    with Not_found ->
      raise (Resolve_error
        (Printf.sprintf "namespace %S not found (referenced by `pub use`)"
           (path_to_string p)))
  in
  let lookup_in (s : module_summary) (item : string) : string option =
    match List.assoc_opt item s.type_names with Some m -> Some m | None ->
    match List.assoc_opt item s.record_names with Some m -> Some m | None ->
    match List.assoc_opt item s.fn_names with Some m -> Some m | None ->
    match List.assoc_opt item s.ctor_names with Some m -> Some m | None ->
    match List.assoc_opt item s.alias_names with Some m -> Some m | None ->
    match List.assoc_opt item s.re_exports with Some m -> Some m | None ->
    if List.mem item s.extern_names then Some item else None
  in
  List.iter (fun (ns_path, prog) ->
    let m_summary = find_summary ns_path in
    List.iter (fun decl ->
      match decl with
      | TopUse u when u.use_pub ->
          let src = find_summary u.use_module in
          List.iter (fun item ->
            match lookup_in src item with
            | Some target ->
                m_summary.re_exports <- (item, target) :: m_summary.re_exports
            | None ->
                raise (Resolve_error
                  (Printf.sprintf
                     "pub use %s::%s — %S is not declared in namespace %S"
                     (path_to_string u.use_module) item item
                     (path_to_string u.use_module)))) u.use_items
      | _ -> ()) prog) per_ns

(* For a namespace M, build the resolution table that maps bare names
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
        try List.find (fun s -> s.mod_path = u.use_module) all_summaries
        with Not_found ->
          raise (Resolve_error
            (Printf.sprintf "namespace %S not found (referenced by `use`)"
               (path_to_string u.use_module)))
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
        match lookup item other.re_exports with
        | Some m -> (item, m)
        | None ->
          if List.mem item other.extern_names then (item, item)
          else raise (Resolve_error
            (Printf.sprintf
               "use %s::%s — %S is not declared in namespace %S"
               (path_to_string u.use_module) item item
               (path_to_string u.use_module)))) u.use_items)
      uses
  in
  let bare_own = List.map fst own in
  List.iter (fun (n, _) ->
    if List.mem n bare_own then
      raise (Resolve_error
        (Printf.sprintf
           "imported name %S clashes with a local declaration in namespace %S"
           n (path_to_string m_summary.mod_path)))) imports;
  let rec check_dups = function
    | [] -> ()
    | (x, _) :: rest ->
        if List.exists (fun (y, _) -> y = x) rest then
          raise (Resolve_error
            (Printf.sprintf "duplicate import of %S in namespace %S"
               x (path_to_string m_summary.mod_path)));
        check_dups rest
  in
  check_dups imports;
  own @ imports

let resolve_name
    (map : (string * string) list) (locals : string list) (name : string)
  : string =
  if String.contains name ':' then
    (* Qualified `mod::item` — mangle to `mod__item`, the same form a
       module's own decls take. Absolute, so the local resolution map is
       not consulted. *)
    String.split_on_char ':' name
    |> List.filter (fun s -> s <> "")
    |> String.concat "__"
  else if List.mem name locals then name
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
  | TyBorrow inner -> TyBorrow (resolve_ty map locals inner)
  | TyTuple ts -> TyTuple (List.map (resolve_ty map locals) ts)
  | TyMeta _ -> t

let rec resolve_expr
    (map : (string * string) list) (locals : string list) (e : expr) : expr =
  let r = resolve_expr map locals in
  let rt = resolve_ty map locals in
  match e with
  | EInt _ | EFloat _ | EBool _ | EStringLit _ -> e
  | EVar x ->
      let resolved = resolve_name map locals x in
      (* A use of a const name becomes a call to its lowered 0-arg fn.
         Locals shadow consts and are left as plain variable refs. *)
      if not (List.mem x locals) && Hashtbl.mem const_set resolved
      then ECall (EVar resolved, [])
      else EVar resolved
  | EBinop (op, a, b) -> EBinop (op, r a, r b)
  | EUnop  (op, a)    -> EUnop  (op, r a)
  | ECall (callee, args) -> ECall (r callee, List.map r args)
  | EFun (params, ret, body) ->
      let params' = List.map (fun (n, t) -> (n, rt t)) params in
      let new_locals = List.map fst params @ locals in
      EFun (params', rt ret, resolve_expr map new_locals body)
  | EClosure (rg, params, ret, body) ->
      let params' = List.map (fun (n, t) -> (n, rt t)) params in
      let new_locals = List.map fst params @ locals in
      EClosure (r rg, params', rt ret, resolve_expr map new_locals body)
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
  | EArena (x, v, body) ->
      let v' = r v in
      let new_locals = x :: locals in
      let body' = resolve_expr map new_locals body in
      EArena (x, v', body')
  | EAssign (x, v) -> EAssign (resolve_name map locals x, r v)
  | EAssignField (p, f, v) -> EAssignField (r p, f, r v)
  | EWhile (c, b) -> EWhile (r c, r b)
  | EBreak | EContinue -> e
  | EReturn v -> EReturn (r v)
  | EMatch (s, arms) ->
      let s' = r s in
      let rec resolve_pat p =
        match p with
        | PCtor (c, vs) -> PCtor (resolve_name map locals c, vs)
        | POr pats -> POr (List.map resolve_pat pats)
        | PTuple ps -> PTuple (List.map resolve_pat ps)
        | PInt _ | PBool _ | PStr _ | PBind _ -> p
      in
      let rec pattern_locals p =
        match p with
        | PCtor (_, vs) -> List.filter (fun v -> v <> "_") vs
        | POr _ -> []
        | PTuple ps -> List.concat_map pattern_locals ps
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
  | EHandle (rg, n, v) -> EHandle (r rg, r n, r v)
  | EHandleLit (rg, elems) -> EHandleLit (r rg, List.map r elems)
  | ERegion n -> ERegion (r n)
  | EStackRegion n -> EStackRegion (r n)
  | EAlignedRegion (n, a) -> EAlignedRegion (r n, r a)
  | EIndex (a, i) -> EIndex (r a, r i)
  | EAssignIdx (a, i, v) -> EAssignIdx (r a, r i, r v)
  | ELen e -> ELen (r e)
  | ESlice (a, lo, hi) -> ESlice (r a, r lo, r hi)
  | ECast (t, e) -> ECast (t, r e)
  | ECAlloc (t, n) -> ECAlloc (rt t, r n)
  | ECFree p -> ECFree (r p)
  | ENullPtr t -> ENullPtr (rt t)
  | EIsNull p -> EIsNull (r p)
  | EHandleData a -> EHandleData (r a)
  | EPtrCast (t, e) -> EPtrCast (rt t, r e)
  | EDeref p -> EDeref (r p)
  | EBorrow p -> EBorrow (r p)
  | ETryAt (a, i) -> ETryAt (r a, r i)
  | EDrop e -> EDrop (r e)
  | EReset e -> EReset (r e)
  | EAwait e -> EAwait (r e)
  | EAwaitAll branches -> EAwaitAll (List.map r branches)
  | EAwaitAllDyn e -> EAwaitAllDyn (r e)
  | ESpawn e -> ESpawn (r e)
  | EForStream (x, src, body) ->
      let src' = r src in
      let new_locals = x :: locals in
      let body' = resolve_expr map new_locals body in
      EForStream (x, src', body')
  | ETuple es -> ETuple (List.map r es)
  | ETupleIdx (e, i) -> ETupleIdx (r e, i)
  | ELetTuple (vs, v, body) ->
      let v' = r v in
      let new_locals =
        List.fold_left (fun acc n -> if n = "_" then acc else n :: acc)
          locals vs
      in
      let body' = resolve_expr map new_locals body in
      ELetTuple (vs, v', body')
  | EPrint (nl, e) -> EPrint (nl, r e)

let resolve_decl
    (map : (string * string) list) (mod_path : string list) (decl : top_decl)
  : top_decl option =
  let m_name n = mangle_for_module mod_path n in
  match decl with
  | TopUse _ -> None
  | TopNamespace _ -> None  (* flattened away upstream *)
  | TopAlias a ->
      Some (TopAlias {
        alias_name = m_name a.alias_name;
        alias_ty   = resolve_ty map [] a.alias_ty;
      })
  | TopConst c ->
      (* Lower a const to a 0-argument function. Uses of the const name
         are rewritten to calls in resolve_expr. *)
      Some (TopFunc {
        name        = m_name c.const_name;
        type_params = [];
        params      = [];
        return_ty   = resolve_ty map [] c.const_ty;
        body        = resolve_expr map [] c.const_value;
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
        is_resource = td.is_resource;
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
        rec_is_resource = rd.rec_is_resource;
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
  | TopTest td ->
      (* Test bodies are normal expressions resolved against the
         module's import map. Test name kept verbatim. *)
      let body = resolve_expr map [] td.test_body in
      Some (TopTest { test_name = td.test_name; test_body = body })

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
  | TyBorrow inner -> TyBorrow (expand_alias_ty aliases visited inner)
  | TyTuple ts -> TyTuple (List.map (expand_alias_ty aliases visited) ts)

let expand_in_expr (aliases : (string * ty) list) (e : expr) : expr =
  let xt t = expand_alias_ty aliases [] t in
  let rec ex e =
    match e with
    | EInt _ | EFloat _ | EBool _ | EStringLit _ | EVar _
    | EBreak | EContinue -> e
    | EBinop (op, a, b) -> EBinop (op, ex a, ex b)
    | EUnop (op, a) -> EUnop (op, ex a)
    | ECall (c, args) -> ECall (ex c, List.map ex args)
    | EFun (params, ret, body) ->
        EFun (List.map (fun (n, t) -> (n, xt t)) params, xt ret, ex body)
    | EClosure (rg, params, ret, body) ->
        EClosure (ex rg, List.map (fun (n, t) -> (n, xt t)) params,
                  xt ret, ex body)
    | ECtor (c, args) -> ECtor (c, List.map ex args)
    | ERecord (n, elems) ->
        ERecord (n, List.map (function
          | RAssign (f, v) -> RAssign (f, ex v)
          | RSpread b -> RSpread (ex b)) elems)
    | EField (e, f) -> EField (ex e, f)
    | EIf (c, t, e) -> EIf (ex c, ex t, ex e)
    | ELet (x, m, asc, v, b) -> ELet (x, m, Option.map xt asc, ex v, ex b)
    | EArena (x, v, b) -> EArena (x, ex v, ex b)
    | EAssign (x, v) -> EAssign (x, ex v)
    | EAssignField (p, f, v) -> EAssignField (ex p, f, ex v)
    | EWhile (c, b) -> EWhile (ex c, ex b)
    | EReturn v -> EReturn (ex v)
    | EMatch (s, arms) ->
        EMatch (ex s, List.map (fun (p, g, b) ->
          (p, Option.map ex g, ex b)) arms)
    | EHandle (r, n, v) -> EHandle (ex r, ex n, ex v)
    | EHandleLit (r, elems) -> EHandleLit (ex r, List.map ex elems)
    | ERegion n -> ERegion (ex n)
    | EStackRegion n -> EStackRegion (ex n)
    | EAlignedRegion (n, a) -> EAlignedRegion (ex n, ex a)
    | EIndex (a, i) -> EIndex (ex a, ex i)
    | EAssignIdx (a, i, v) -> EAssignIdx (ex a, ex i, ex v)
    | ELen e -> ELen (ex e)
    | ESlice (a, lo, hi) -> ESlice (ex a, ex lo, ex hi)
    | ECast (t, e) -> ECast (t, ex e)
    | ECAlloc (t, n) -> ECAlloc (xt t, ex n)
    | ECFree p -> ECFree (ex p)
    | ENullPtr t -> ENullPtr (xt t)
    | EIsNull p -> EIsNull (ex p)
    | EHandleData a -> EHandleData (ex a)
    | EPtrCast (t, e) -> EPtrCast (xt t, ex e)
    | EDeref p -> EDeref (ex p)
    | EBorrow p -> EBorrow (ex p)
    | ETryAt (a, i) -> ETryAt (ex a, ex i)
    | EDrop e -> EDrop (ex e)
    | EReset e -> EReset (ex e)
    | EAwait e -> EAwait (ex e)
    | EAwaitAll branches -> EAwaitAll (List.map ex branches)
    | EAwaitAllDyn e -> EAwaitAllDyn (ex e)
    | ESpawn e -> ESpawn (ex e)
    | EForStream (x, s, b) -> EForStream (x, ex s, ex b)
    | ETuple es -> ETuple (List.map ex es)
    | ETupleIdx (e, i) -> ETupleIdx (ex e, i)
    | ELetTuple (vs, v, b) -> ELetTuple (vs, ex v, ex b)
    | EPrint (nl, e) -> EPrint (nl, ex e)
  in
  ex e

let expand_in_decl (aliases : (string * ty) list) (d : top_decl) : top_decl =
  let xt t = expand_alias_ty aliases [] t in
  match d with
  | TopType td ->
      TopType { td with variants =
        List.map (fun v ->
          { v with arg_tys = List.map xt v.arg_tys }) td.variants;
        is_resource = td.is_resource; }
  | TopRecord rd ->
      TopRecord { rd with rec_fields =
        List.map (fun (f, t) -> (f, xt t)) rd.rec_fields;
        rec_is_resource = rd.rec_is_resource; }
  | TopFunc f ->
      TopFunc { f with
        params = List.map (fun (n, t) -> (n, xt t)) f.params;
        return_ty = xt f.return_ty;
        body = expand_in_expr aliases f.body; }
  | TopExtern e ->
      TopExtern { e with
        ext_params = List.map (fun (n, t) -> (n, xt t)) e.ext_params;
        ext_return_ty = xt e.ext_return_ty; }
  | TopTest td -> TopTest { td with test_body = expand_in_expr aliases td.test_body }
  | TopUse _ | TopAlias _ | TopConst _ -> d
  | TopNamespace _ -> d

(* Top-level entry: take an ordered list of (file_name, parsed program),
   flatten namespace blocks into per-namespace decl lists, resolve all
   references, return one merged program ready for the type checker.
   Externs are deduplicated by name. *)
let resolve (modules : (string * program) list) : program =
  Hashtbl.clear const_set;
  (* Step 1: each file → list of (namespace_path, decls). Multiple
     files contributing to the same namespace path are merged. *)
  let merged : (string list * top_decl list) list =
    let acc : (string list, top_decl list ref) Hashtbl.t = Hashtbl.create 8 in
    let order : string list list ref = ref [] in
    List.iter (fun (fn, prog) ->
      let per_ns = flatten_namespaces fn prog in
      List.iter (fun (path, decls) ->
        match Hashtbl.find_opt acc path with
        | Some r -> r := !r @ decls
        | None ->
            Hashtbl.add acc path (ref decls);
            order := path :: !order) per_ns) modules;
    List.rev_map (fun path -> (path, !(Hashtbl.find acc path))) !order
  in
  let summaries = List.map (fun (p, decls) -> summarize p decls) merged in
  fill_re_exports summaries merged;
  let resolved_per_module = List.map (fun (path, prog) ->
    let m_summary = List.find (fun s -> s.mod_path = path) summaries in
    let uses = List.filter_map (function
      | TopUse u -> Some u
      | _ -> None) prog
    in
    let map = build_resolution_map m_summary uses summaries in
    List.filter_map (resolve_decl map path) prog
  ) merged
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

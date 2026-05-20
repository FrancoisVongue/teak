(* Lambda lifting.

   Runs after resolve, before check. Removes every [EFun] anonymous
   function literal from the program by hoisting it to a fresh top-level
   function and replacing the literal with a reference to that function.

   Iteration 1: capture-free only. A lambda may reference its own
   parameters and any global name (top-level function or extern), but
   not a local binding from an enclosing scope. A captured local is
   rejected with a clear error — closure environments come in a later
   iteration. *)

open Ast

exception Lift_error of string

module SS = Set.Make (String)

(* ---------- free variables of an expression ---------- *)

let pattern_vars (p : pat) : string list =
  let rec go = function
    | PCtor (_, vs) -> List.filter (fun v -> v <> "_") vs
    | POr _         -> []
    | PTuple ps     -> List.concat_map go ps
    | PInt _ | PBool _ | PStr _ -> []
    | PBind "_"     -> []
    | PBind x       -> [x]
  in
  go p

let rec free_vars (e : expr) : SS.t =
  let u = List.fold_left SS.union SS.empty in
  match e with
  | EInt _ | EFloat _ | EBool _ | EStringLit _ -> SS.empty
  | EVar x -> SS.singleton x
  | EBinop (_, a, b) -> SS.union (free_vars a) (free_vars b)
  | EUnop  (_, a) -> free_vars a
  | ECall (callee, args) -> u (free_vars callee :: List.map free_vars args)
  | EFun (params, _, body) ->
      SS.diff (free_vars body) (SS.of_list (List.map fst params))
  | EClosure (r, params, _, body) ->
      SS.union (free_vars r)
        (SS.diff (free_vars body) (SS.of_list (List.map fst params)))
  | ECtor (_, args) -> u (List.map free_vars args)
  | ERecord (_, elems) ->
      u (List.map (function
           | RAssign (_, v) -> free_vars v
           | RSpread b      -> free_vars b) elems)
  | EField (e, _) -> free_vars e
  | EIf (c, t, el) -> u [free_vars c; free_vars t; free_vars el]
  | ELet (x, _, _, v, body) ->
      SS.union (free_vars v) (SS.remove x (free_vars body))
  | EAssign (x, v) -> SS.add x (free_vars v)
  | EAssignField (p, _, v) -> SS.union (free_vars p) (free_vars v)
  | EWhile (c, b) -> SS.union (free_vars c) (free_vars b)
  | EBreak | EContinue -> SS.empty
  | EReturn e -> free_vars e
  | EMatch (s, arms) ->
      let arm_fv (p, guard, body) =
        let bound = SS.of_list (pattern_vars p) in
        let g = match guard with Some g -> free_vars g | None -> SS.empty in
        SS.diff (SS.union g (free_vars body)) bound
      in
      u (free_vars s :: List.map arm_fv arms)
  | EArray (r, n, v) -> u [free_vars r; free_vars n; free_vars v]
  | EArrayLit (r, elems) -> u (free_vars r :: List.map free_vars elems)
  | ERegion n | EStackRegion n -> free_vars n
  | EAlignedRegion (n, a) -> SS.union (free_vars n) (free_vars a)
  | EIndex (a, i) -> SS.union (free_vars a) (free_vars i)
  | EAssignIdx (a, i, v) -> u [free_vars a; free_vars i; free_vars v]
  | ELen e -> free_vars e
  | ESlice (a, lo, hi) -> u [free_vars a; free_vars lo; free_vars hi]
  | EToInt e | EToByte e | EToU16 e | EToU32 e | EToU64 e | EToFloat e ->
      free_vars e
  | ECAlloc (_, n) -> free_vars n
  | ECFree p | EIsNull p | EArrayData p | EDeref p -> free_vars p
  | ENullPtr _ -> SS.empty
  | ETryAt (a, i) -> SS.union (free_vars a) (free_vars i)
  | EDrop e -> free_vars e
  | EAwait e | EAwaitAllDyn e | ESpawn e -> free_vars e
  | EAwaitAll branches -> u (List.map free_vars branches)
  | EForStream (x, src, body) ->
      SS.union (free_vars src) (SS.remove x (free_vars body))
  | ETuple es -> u (List.map free_vars es)
  | ETupleIdx (e, _) -> free_vars e
  | ELetTuple (vs, v, body) ->
      SS.union (free_vars v) (SS.diff (free_vars body) (SS.of_list vs))
  | EArena (x, v, body) ->
      SS.union (free_vars v) (SS.remove x (free_vars body))
  | EPrint (_, e) -> free_vars e

(* ---------- the lift pass ---------- *)

let lift (prog : program) : program =
  (* Names visible at the top level — a free variable resolving to one
     of these is fine (a function/extern reference), not a capture.
     The program is already flat (resolve removed namespaces). *)
  let globals : (string, unit) Hashtbl.t = Hashtbl.create 64 in
  List.iter (function
    | TopFunc f   -> Hashtbl.replace globals f.name ()
    | TopExtern e -> Hashtbl.replace globals e.ext_name ()
    | _ -> ()) prog;

  let lifted : func list ref = ref [] in
  let counter = ref 0 in

  let rec xform (e : expr) : expr =
    match e with
    | EFun (params, ret, body) ->
        let body' = xform body in
        let param_names = SS.of_list (List.map fst params) in
        let captures =
          SS.filter
            (fun v -> not (SS.mem v param_names) && not (Hashtbl.mem globals v))
            (free_vars body')
        in
        if not (SS.is_empty captures) then
          raise (Lift_error (Printf.sprintf
            "lambda captures local variable(s) %s — a bare `fn(...)` cannot \
             capture; wrap it as `closure(r, fn(...))` to store the captured \
             environment in region r"
            (String.concat ", " (SS.elements captures))));
        let name = Printf.sprintf "__lambda_%d" !counter in
        incr counter;
        Hashtbl.replace globals name ();
        lifted :=
          { name; type_params = []; params; return_ty = ret; body = body' }
          :: !lifted;
        EVar name
    | EClosure (r, params, ret, body) ->
        (* Leave the closure for the checker (it needs types to build
           the environment); just lift any nested capture-free lambdas
           inside the region expr and the body. *)
        EClosure (xform r, params, ret, xform body)
    | EInt _ | EFloat _ | EBool _ | EStringLit _ | EVar _
    | EBreak | EContinue | ENullPtr _ -> e
    | EBinop (op, a, b) -> EBinop (op, xform a, xform b)
    | EUnop (op, a) -> EUnop (op, xform a)
    | ECall (callee, args) -> ECall (xform callee, List.map xform args)
    | ECtor (c, args) -> ECtor (c, List.map xform args)
    | ERecord (n, elems) ->
        ERecord (n, List.map (function
          | RAssign (f, v) -> RAssign (f, xform v)
          | RSpread b      -> RSpread (xform b)) elems)
    | EField (e, f) -> EField (xform e, f)
    | EIf (c, t, el) -> EIf (xform c, xform t, xform el)
    | ELet (x, m, asc, v, body) -> ELet (x, m, asc, xform v, xform body)
    | EAssign (x, v) -> EAssign (x, xform v)
    | EAssignField (p, f, v) -> EAssignField (xform p, f, xform v)
    | EWhile (c, b) -> EWhile (xform c, xform b)
    | EReturn e -> EReturn (xform e)
    | EMatch (s, arms) ->
        EMatch (xform s,
          List.map (fun (p, g, b) ->
            (p, Option.map xform g, xform b)) arms)
    | EArray (r, n, v) -> EArray (xform r, xform n, xform v)
    | EArrayLit (r, elems) -> EArrayLit (xform r, List.map xform elems)
    | ERegion n -> ERegion (xform n)
    | EStackRegion n -> EStackRegion (xform n)
    | EAlignedRegion (n, a) -> EAlignedRegion (xform n, xform a)
    | EIndex (a, i) -> EIndex (xform a, xform i)
    | EAssignIdx (a, i, v) -> EAssignIdx (xform a, xform i, xform v)
    | ELen e -> ELen (xform e)
    | ESlice (a, lo, hi) -> ESlice (xform a, xform lo, xform hi)
    | EToInt e -> EToInt (xform e)
    | EToByte e -> EToByte (xform e)
    | EToU16 e -> EToU16 (xform e)
    | EToU32 e -> EToU32 (xform e)
    | EToU64 e -> EToU64 (xform e)
    | EToFloat e -> EToFloat (xform e)
    | ECAlloc (t, n) -> ECAlloc (t, xform n)
    | ECFree p -> ECFree (xform p)
    | EIsNull p -> EIsNull (xform p)
    | EArrayData a -> EArrayData (xform a)
    | ETryAt (a, i) -> ETryAt (xform a, xform i)
    | EDrop e -> EDrop (xform e)
    | EDeref p -> EDeref (xform p)
    | EAwait e -> EAwait (xform e)
    | EAwaitAll branches -> EAwaitAll (List.map xform branches)
    | EAwaitAllDyn e -> EAwaitAllDyn (xform e)
    | ESpawn e -> ESpawn (xform e)
    | EForStream (x, src, body) -> EForStream (x, xform src, xform body)
    | ETuple es -> ETuple (List.map xform es)
    | ETupleIdx (e, i) -> ETupleIdx (xform e, i)
    | ELetTuple (vs, v, body) -> ELetTuple (vs, xform v, xform body)
    | EArena (x, v, body) -> EArena (x, xform v, xform body)
    | EPrint (nl, e) -> EPrint (nl, xform e)
  in

  let xform_decl = function
    | TopFunc f -> TopFunc { f with body = xform f.body }
    | TopTest t -> TopTest { t with test_body = xform t.test_body }
    | d -> d
  in
  let prog' = List.map xform_decl prog in
  prog' @ List.map (fun f -> TopFunc f) (List.rev !lifted)

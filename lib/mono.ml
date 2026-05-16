(* Monomorphization.

   Walks the typed program from `main`. Generates one specialized
   copy of each polymorphic function/ADT per instantiation. Output
   has no TyVar; ADTs are referenced by mangled names. TyFun stays
   structural (the emitter typedefs each distinct function type). *)

open Ast

(* ---------- name mangling ---------- *)

let rec mangle_ty (t : ty) : string =
  match t with
  | TyInt          -> "int"
  | TyBool         -> "bool"
  | TyVar n        ->
      failwith (Printf.sprintf
        "mono: unresolved type variable %S" n)
  | TyApp (n, [])  -> n
  | TyApp (n, args) ->
      n ^ "_" ^ String.concat "_" (List.map mangle_ty args)
  | TyFun (args, ret) ->
      let parts =
        if args = [] then ["fn"; "to"; mangle_ty ret]
        else ["fn"]
             @ (List.map mangle_ty args)
             @ ["to"; mangle_ty ret]
      in
      String.concat "_" parts
  | TyMeta _ -> failwith "mono: TyMeta after check"

let mangle_name (name : string) (ts : ty list) : string =
  if ts = [] then name
  else name ^ "_" ^ String.concat "_" (List.map mangle_ty ts)

(* ---------- the pass ---------- *)

let monomorphize (prog : Check.T.program) : Check.T.program =
  let fn_queue : (string * ty list) Queue.t = Queue.create () in
  let mono_fns : (string, Check.T.func) Hashtbl.t = Hashtbl.create 16 in
  let fn_seen  : (string * ty list, unit) Hashtbl.t = Hashtbl.create 16 in

  let adt_queue : (string * ty list) Queue.t = Queue.create () in
  let mono_adts : (string, type_decl) Hashtbl.t = Hashtbl.create 16 in
  let adt_seen  : (string * ty list, unit) Hashtbl.t = Hashtbl.create 16 in

  let rec_queue : (string * ty list) Queue.t = Queue.create () in
  let mono_recs : (string, record_decl) Hashtbl.t = Hashtbl.create 16 in
  let rec_seen  : (string * ty list, unit) Hashtbl.t = Hashtbl.create 16 in

  let extern_names : (string, unit) Hashtbl.t =
    Hashtbl.create (List.length prog.externs)
  in
  List.iter (fun (e : Check.T.extern) ->
    Hashtbl.replace extern_names e.name ()) prog.externs;
  let is_extern name = Hashtbl.mem extern_names name in

  let request_fn name ts =
    if not (Hashtbl.mem fn_seen (name, ts)) then begin
      Hashtbl.add fn_seen (name, ts) ();
      Queue.add (name, ts) fn_queue
    end
  in
  let request_adt name ts =
    if not (Hashtbl.mem adt_seen (name, ts)) then begin
      Hashtbl.add adt_seen (name, ts) ();
      Queue.add (name, ts) adt_queue
    end
  in
  let request_rec name ts =
    if not (Hashtbl.mem rec_seen (name, ts)) then begin
      Hashtbl.add rec_seen (name, ts) ();
      Queue.add (name, ts) rec_queue
    end
  in
  let record_names : (string, unit) Hashtbl.t =
    Hashtbl.create (List.length prog.records)
  in
  List.iter (fun (rd : record_decl) ->
    Hashtbl.replace record_names rd.rec_name ()) prog.records;
  let is_record_name n = Hashtbl.mem record_names n in

  let rec rewrite_ty (subst : (string * ty) list) (t : ty) : ty =
    match t with
    | TyInt | TyBool -> t
    | TyVar n ->
        (try List.assoc n subst
         with Not_found ->
           failwith (Printf.sprintf
             "mono rewrite_ty: free type variable %S" n))
    | TyApp ("Ref", [inner]) ->
        (* Ref is structural — no body to specialize. Just rewrite its
           inner type. Emit will collect Ref instantiations and produce
           one typedef per distinct one. *)
        TyApp ("Ref", [rewrite_ty subst inner])
    | TyApp ("Ref", _) ->
        failwith "mono rewrite_ty: Ref with wrong arity (should be unary)"
    | TyApp ("Own", [inner]) ->
        (* Own is structural like Ref. *)
        TyApp ("Own", [rewrite_ty subst inner])
    | TyApp ("Own", _) ->
        failwith "mono rewrite_ty: Own with wrong arity (should be unary)"
    | TyApp ("Array", [inner]) ->
        (* Array is also structural — emit emits one cell+wrapper pair
           per distinct element type. *)
        TyApp ("Array", [rewrite_ty subst inner])
    | TyApp ("Array", _) ->
        failwith "mono rewrite_ty: Array with wrong arity (should be unary)"
    | TyApp (n, args) ->
        let args = List.map (rewrite_ty subst) args in
        if is_record_name n then request_rec n args
        else request_adt n args;
        TyApp (mangle_name n args, [])
    | TyFun (args, ret) ->
        TyFun (List.map (rewrite_ty subst) args, rewrite_ty subst ret)
    | TyMeta _ ->
        failwith "mono rewrite_ty: TyMeta after checking"
  in

  let rec rewrite_expr (subst : (string * ty) list)
    (e : Check.T.expr) : Check.T.expr =
    let rt = rewrite_ty subst in
    match e with
    | Check.T.TEInt _ | Check.T.TEBool _ -> e
    | Check.T.TEVar (x, t) -> Check.T.TEVar (x, rt t)

    | Check.T.TEFnRef (name, ts, fn_ty) ->
        let ts = List.map rt ts in
        let fn_ty = rt fn_ty in
        if is_extern name then
          Check.T.TEFnRef (name, [], fn_ty)
        else begin
          request_fn name ts;
          Check.T.TEFnRef (mangle_name name ts, [], fn_ty)
        end

    | Check.T.TECall (callee, args, ret) ->
        Check.T.TECall (rewrite_expr subst callee,
                        List.map (rewrite_expr subst) args,
                        rt ret)

    | Check.T.TEBinop (op, a, b, ty) ->
        Check.T.TEBinop (op,
                         rewrite_expr subst a,
                         rewrite_expr subst b, rt ty)

    | Check.T.TEUnop (op, e, ty) ->
        Check.T.TEUnop (op, rewrite_expr subst e, rt ty)

    | Check.T.TECtor (c, _ts, args, ret) ->
        let args = List.map (rewrite_expr subst) args in
        let ret  = rt ret in
        Check.T.TECtor (c, [], args, ret)

    | Check.T.TERecord (_name, _ts, fields, ret) ->
        let fields = List.map (fun (fn, e) ->
          (fn, rewrite_expr subst e)) fields in
        let ret = rt ret in
        (* result type already encodes the (mangled) record name *)
        let mangled = match ret with
          | TyApp (n, []) -> n
          | _ -> failwith "mono: record result not a flat TyApp"
        in
        Check.T.TERecord (mangled, [], fields, ret)

    | Check.T.TEField (e, fname, fty) ->
        Check.T.TEField (rewrite_expr subst e, fname, rt fty)

    | Check.T.TEIf (c, t, e2, ty) ->
        Check.T.TEIf (rewrite_expr subst c,
                      rewrite_expr subst t,
                      rewrite_expr subst e2, rt ty)
    | Check.T.TELet (x, vt, v, b, bt, ad) ->
        Check.T.TELet (x, rt vt,
                       rewrite_expr subst v,
                       rewrite_expr subst b, rt bt, ad)
    | Check.T.TEMatch (s, st, arms, rty) ->
        let arms = List.map (fun (p, b) ->
          (p, rewrite_expr subst b)) arms in
        Check.T.TEMatch (rewrite_expr subst s, rt st, arms, rt rty)

    | Check.T.TERef (e, t) ->
        Check.T.TERef (rewrite_expr subst e, rt t)
    | Check.T.TEDeref (e, t) ->
        Check.T.TEDeref (rewrite_expr subst e, rt t)
    | Check.T.TEAssign (r, v, t) ->
        Check.T.TEAssign (rewrite_expr subst r,
                          rewrite_expr subst v, rt t)
    | Check.T.TEPanic t ->
        Check.T.TEPanic (rt t)

    | Check.T.TEOwn (e, t) ->
        Check.T.TEOwn (rewrite_expr subst e, rt t)
    | Check.T.TETake (e, t) ->
        Check.T.TETake (rewrite_expr subst e, rt t)
    | Check.T.TEUnwrap (e, t) ->
        Check.T.TEUnwrap (rewrite_expr subst e, rt t)
    | Check.T.TELook (e, t) ->
        Check.T.TELook (rewrite_expr subst e, rt t)
    | Check.T.TEArray (n, v, t) ->
        Check.T.TEArray (rewrite_expr subst n, rewrite_expr subst v, rt t)
    | Check.T.TEIndex (a, i, t) ->
        Check.T.TEIndex (rewrite_expr subst a, rewrite_expr subst i, rt t)
    | Check.T.TEAssignIdx (a, i, v, t) ->
        Check.T.TEAssignIdx (rewrite_expr subst a,
                             rewrite_expr subst i,
                             rewrite_expr subst v, rt t)
    | Check.T.TELen (e, t) ->
        Check.T.TELen (rewrite_expr subst e, rt t)
  in

  request_fn "main" [];

  (* Indexes for O(1) lookup of original fns/types by name. *)
  let orig_fns_idx : (string, Check.T.func) Hashtbl.t =
    Hashtbl.create (List.length prog.funcs)
  in
  List.iter (fun (f : Check.T.func) ->
    Hashtbl.replace orig_fns_idx f.name f) prog.funcs;
  let orig_adts_idx : (string, type_decl) Hashtbl.t =
    Hashtbl.create (List.length prog.types)
  in
  List.iter (fun td ->
    Hashtbl.replace orig_adts_idx td.type_name td) prog.types;

  let orig_recs_idx : (string, record_decl) Hashtbl.t =
    Hashtbl.create (List.length prog.records)
  in
  List.iter (fun (rd : record_decl) ->
    Hashtbl.replace orig_recs_idx rd.rec_name rd) prog.records;

  let rec drain () =
    if Queue.is_empty fn_queue
       && Queue.is_empty adt_queue
       && Queue.is_empty rec_queue then ()
    else begin
      while not (Queue.is_empty fn_queue) do
        let (name, ts) = Queue.pop fn_queue in
        let orig =
          try Hashtbl.find orig_fns_idx name
          with Not_found ->
            failwith (Printf.sprintf "mono: function %S not found" name)
        in
        let subst = List.combine orig.type_params ts in
        let new_params =
          List.map (fun (n, t) -> (n, rewrite_ty subst t)) orig.params
        in
        let new_ret  = rewrite_ty subst orig.return_ty in
        let new_body = rewrite_expr subst orig.body in
        let mono : Check.T.func = {
          name        = mangle_name name ts;
          type_params = [];
          params      = new_params;
          return_ty   = new_ret;
          body        = new_body;
        } in
        Hashtbl.replace mono_fns mono.name mono
      done;
      while not (Queue.is_empty adt_queue) do
        let (name, ts) = Queue.pop adt_queue in
        let orig =
          try Hashtbl.find orig_adts_idx name
          with Not_found ->
            failwith (Printf.sprintf "mono: type %S not found" name)
        in
        let subst = List.combine orig.type_params ts in
        let new_variants = List.map (fun v ->
          { v with arg_tys = List.map (rewrite_ty subst) v.arg_tys })
          orig.variants
        in
        let mono = {
          type_name   = mangle_name name ts;
          type_params = [];
          variants    = new_variants;
        } in
        Hashtbl.replace mono_adts mono.type_name mono
      done;
      while not (Queue.is_empty rec_queue) do
        let (name, ts) = Queue.pop rec_queue in
        let orig =
          try Hashtbl.find orig_recs_idx name
          with Not_found ->
            failwith (Printf.sprintf "mono: record %S not found" name)
        in
        let subst = List.combine orig.rec_type_params ts in
        let new_fields = List.map (fun (fn, fty) ->
          (fn, rewrite_ty subst fty)) orig.rec_fields
        in
        let mono = {
          rec_name        = mangle_name name ts;
          rec_type_params = [];
          rec_fields      = new_fields;
        } in
        Hashtbl.replace mono_recs mono.rec_name mono
      done;
      drain ()
    end
  in
  drain ();

  let final_funcs =
    Hashtbl.fold (fun _ v acc -> v :: acc) mono_fns []
  in
  let final_types =
    Hashtbl.fold (fun _ v acc -> v :: acc) mono_adts []
  in
  let final_records =
    Hashtbl.fold (fun _ v acc -> v :: acc) mono_recs []
  in
  { Check.T.types   = final_types;
    Check.T.records = final_records;
    Check.T.funcs   = final_funcs;
    Check.T.externs = prog.externs }

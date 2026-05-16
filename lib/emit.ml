(* Emit C from the monomorphized typed AST.

   Input has no type parameters and no TyVar. ADT references use
   mangled names (`Option_int`). TyFun is kept structural; we
   typedef each unique function-pointer type once.

   Output structure:
     1. Forward declarations of all ADTs (so fn-pointer typedefs
        can mention them by name).
     2. Fn-pointer typedefs.
     3. Full ADT struct definitions.
     4. Function forward declarations.
     5. Function definitions.

   Expression strategy: ANF-style temporaries for if/match. Operators
   are first-class AST nodes (TEBinop/TEUnop), each emitted as the
   corresponding C operator inline. *)

open Ast

type c_code = {
  stmts : string list;
  value : string;
}

let counter = ref 0
let fresh prefix =
  incr counter;
  Printf.sprintf "%s_%d" prefix !counter
let reset_counter () = counter := 0

(* ---------- collect distinct TyFun types ---------- *)

(* Mangled name -> the structural TyFun. Used to emit a typedef per
   unique function type. We track insertion order alongside the set
   for dedup: a function type T may reference inner function types,
   and those inner typedefs must come BEFORE T's. collect_ty visits
   children before registering parents, so prepending to the list
   gives us [newest; ...; oldest] = [parent; ...; deepest_child].
   Reversing on emit yields the right order for C. *)
let fn_types_seen : (string, unit) Hashtbl.t = Hashtbl.create 16
let fn_types_order : (string * ty) list ref = ref []

(* Array[T] instantiations: emit one typedef per distinct element type. *)
let array_types_seen : (string, unit) Hashtbl.t = Hashtbl.create 8
let array_types_order : (string * ty) list ref = ref []

let mangle_array_name (inner : ty) : string =
  "Array_" ^ Mono.mangle_ty inner

let rec collect_ty (t : ty) : unit =
  match t with
  | TyInt | TyBool -> ()
  | TyVar n ->
      failwith (Printf.sprintf "emit collect_ty: TyVar %S after mono" n)
  | TyApp ("Array", [inner]) ->
      collect_ty inner;
      let m = mangle_array_name inner in
      if not (Hashtbl.mem array_types_seen m) then begin
        Hashtbl.add array_types_seen m ();
        array_types_order := (m, inner) :: !array_types_order
      end
  | TyApp ("Array", _) ->
      failwith "emit collect_ty: Array with wrong arity"
  | TyApp ("Region", []) -> ()
      (* Region runtime is emitted unconditionally at the top of the file. *)
  | TyApp ("Region", _) ->
      failwith "emit collect_ty: Region takes no type arguments"
  | TyApp (_, []) -> ()
  | TyApp (n, _) ->
      failwith (Printf.sprintf "emit collect_ty: %S still has args" n)
  | TyFun (args, ret) ->
      List.iter collect_ty args;
      collect_ty ret;
      let m = Mono.mangle_ty t in
      if not (Hashtbl.mem fn_types_seen m) then begin
        Hashtbl.add fn_types_seen m ();
        fn_types_order := (m, t) :: !fn_types_order
      end
  | TyMeta _ -> failwith "emit collect_ty: TyMeta"

let rec collect_expr (e : Check.T.expr) : unit =
  match e with
  | Check.T.TEInt _ | Check.T.TEBool _ -> ()
  | Check.T.TEVar (_, t) -> collect_ty t
  | Check.T.TEFnRef (_, _, t) -> collect_ty t
  | Check.T.TECall (callee, args, t) ->
      collect_expr callee;
      List.iter collect_expr args;
      collect_ty t
  | Check.T.TEBinop (_, a, b, t) ->
      collect_expr a; collect_expr b; collect_ty t
  | Check.T.TEUnop (_, e, t) ->
      collect_expr e; collect_ty t
  | Check.T.TECtor (_, _, args, t) ->
      List.iter collect_expr args; collect_ty t
  | Check.T.TERecord (_, _, fields, t) ->
      List.iter (fun (_, e) -> collect_expr e) fields;
      collect_ty t
  | Check.T.TEField (e, _, t) ->
      collect_expr e; collect_ty t
  | Check.T.TEIf (c, t, e, ty) ->
      collect_expr c; collect_expr t; collect_expr e; collect_ty ty
  | Check.T.TELet (_, vt, v, b, bt, _) ->
      collect_ty vt; collect_expr v; collect_expr b; collect_ty bt
  | Check.T.TEMatch (s, st, arms, rt) ->
      collect_expr s; collect_ty st;
      List.iter (fun (_, body) -> collect_expr body) arms;
      collect_ty rt
  | Check.T.TEArray (r, n, v, t) ->
      collect_expr r; collect_expr n; collect_expr v; collect_ty t
  | Check.T.TEArrayLit (r, elems, t) ->
      collect_expr r; List.iter collect_expr elems; collect_ty t
  | Check.T.TERegion (n, t) -> collect_expr n; collect_ty t
  | Check.T.TEIndex (a, i, t) ->
      collect_expr a; collect_expr i; collect_ty t
  | Check.T.TEAssignIdx (a, i, v, t) ->
      collect_expr a; collect_expr i; collect_expr v; collect_ty t
  | Check.T.TELen (e, t) -> collect_expr e; collect_ty t

let collect_program (prog : Check.T.program) : unit =
  Hashtbl.clear fn_types_seen;
  fn_types_order := [];
  Hashtbl.clear array_types_seen;
  array_types_order := [];
  List.iter (fun td ->
    List.iter (fun v ->
      List.iter collect_ty v.arg_tys) td.variants) prog.types;
  List.iter (fun (rd : record_decl) ->
    List.iter (fun (_, t) -> collect_ty t) rd.rec_fields) prog.records;
  List.iter (fun (e : Check.T.extern) ->
    List.iter (fun (_, t) -> collect_ty t) e.params;
    collect_ty e.return_ty) prog.externs;
  List.iter (fun (f : Check.T.func) ->
    List.iter (fun (_, t) -> collect_ty t) f.params;
    collect_ty f.return_ty;
    collect_expr f.body) prog.funcs

(* ---------- rendering C types ---------- *)

let c_type (t : ty) : string =
  match t with
  | TyInt -> "int"
  | TyBool -> "int"
  | TyApp ("Array", [inner]) -> mangle_array_name inner
  | TyApp ("Region", []) -> "Region"
  | TyApp (n, []) -> n
  | TyFun _ -> Mono.mangle_ty t
  | TyApp (n, _) ->
      failwith (Printf.sprintf "emit: %S still has type args" n)
  | TyVar n ->
      failwith (Printf.sprintf "emit: TyVar %S after mono" n)
  | TyMeta _ -> failwith "emit: TyMeta after mono"

let emit_fn_typedefs () : string list =
  (* Emit in reverse-insertion order = oldest first = deepest child first. *)
  List.rev_map (fun (mangled, t) ->
    match t with
    | TyFun (args, ret) ->
        let ret_c = c_type ret in
        if args = [] then
          Printf.sprintf "typedef %s (*%s)(void);" ret_c mangled
        else
          Printf.sprintf "typedef %s (*%s)(%s);" ret_c mangled
            (String.concat ", " (List.map c_type args))
    | _ -> failwith "emit_fn_typedefs: non-fn type in list")
    !fn_types_order

(* Array_T is a copyable handle into a Region's buffer: it carries the
   region header pointer, the byte-offset of this array's slice in
   that buffer, the length, and the expected region generation. *)
let emit_array_forwards () : string list =
  List.rev_map (fun (mangled, _inner) ->
    Printf.sprintf
      "typedef struct { struct Region_header* region; int offset; \
       int len; int expected_gen; } %s;" mangled)
    !array_types_order

(* ---------- operator C-strings ---------- *)

let c_binop = function
  | OpAdd -> "+"  | OpSub -> "-"
  | OpMul -> "*"  | OpDiv -> "/"  | OpMod -> "%"
  | OpEq  -> "==" | OpNeq -> "!="
  | OpLt  -> "<"  | OpGt  -> ">"
  | OpLe  -> "<=" | OpGe  -> ">="
  | OpAnd -> "&&" | OpOr  -> "||"

let c_unop = function
  | OpNeg -> "-"
  | OpNot -> "!"

(* ---------- ADT emission ---------- *)

let emit_adt_forward (td : type_decl) : string =
  Printf.sprintf "typedef struct %s %s;" td.type_name td.type_name

(* Alpha-rename let- and match-arm-bindings so every local binding in a C
   function body has a unique name. Source allows shadowing (`let s = ...;
   let s = ...;`), but C does not — multiple declarations of the same
   identifier in the same block are an error. Renaming to globally unique
   names sidesteps this without introducing nested C blocks (which would
   complicate the value-of-block protocol we use for if/match). *)

let alpha_rename_func (f : Check.T.func) : Check.T.func =
  let counter = ref 0 in
  let fresh name =
    incr counter;
    Printf.sprintf "%s_%d" name !counter
  in
  let rec rn (env : (string * string) list) (e : Check.T.expr) : Check.T.expr =
    let open Check.T in
    match e with
    | TEInt _ | TEBool _ | TEFnRef _ -> e
    | TEVar (x, t) ->
        let x' = try List.assoc x env with Not_found -> x in
        TEVar (x', t)
    | TECall (callee, args, t) ->
        TECall (rn env callee, List.map (rn env) args, t)
    | TEBinop (op, a, b, t) -> TEBinop (op, rn env a, rn env b, t)
    | TEUnop  (op, a, t)    -> TEUnop  (op, rn env a, t)
    | TECtor (c, ts, args, t) ->
        TECtor (c, ts, List.map (rn env) args, t)
    | TERecord (n, ts, fields, t) ->
        TERecord (n, ts,
          List.map (fun (fn, e) -> (fn, rn env e)) fields, t)
    | TEField (e, fn, t) -> TEField (rn env e, fn, t)
    | TEIf (c, th, el, t) -> TEIf (rn env c, rn env th, rn env el, t)
    | TELet (x, vt, v, body, bt, ad) ->
        let v' = rn env v in
        if x = "_" then
          TELet ("_", vt, v', rn env body, bt, ad)
        else
          let x' = fresh x in
          let env' = (x, x') :: env in
          TELet (x', vt, v', rn env' body, bt, ad)
    | TEMatch (s, st, arms, rt) ->
        let s' = rn env s in
        let arms' =
          List.map (fun (p, body) ->
            match p with
            | PWild -> (p, rn env body)
            | PCtor (c, names) ->
                let pairs =
                  List.map (fun n ->
                    if n = "_" then (n, n) else (n, fresh n)) names
                in
                let new_names = List.map snd pairs in
                let env' = pairs @ env in
                (PCtor (c, new_names), rn env' body)) arms
        in
        TEMatch (s', st, arms', rt)
    | TEArray (r, n, v, t) ->
        TEArray (rn env r, rn env n, rn env v, t)
    | TEArrayLit (r, elems, t) ->
        TEArrayLit (rn env r, List.map (rn env) elems, t)
    | TERegion (n, t) -> TERegion (rn env n, t)
    | TEIndex (a, i, t) -> TEIndex (rn env a, rn env i, t)
    | TEAssignIdx (a, i, v, t) ->
        TEAssignIdx (rn env a, rn env i, rn env v, t)
    | TELen (e, t) -> TELen (rn env e, t)
  in
  let initial_env = List.map (fun (p, _) -> (p, p)) f.params in
  { f with body = rn initial_env f.body }

(* ---------- struct topology ---------- *)

(* Топологическая сортировка определений struct-типов (ADT + records).

   В C при определении struct A с полем типа B по значению необходимо
   чтобы B было полностью определено ДО A. Forward-declaration не
   хватает: компилятор должен знать размер B чтобы разместить его
   как поле.

   Функтипы — это указатели фиксированного размера, они не создают
   "by value" зависимости, поэтому игнорируются в графе. *)

type struct_decl =
  | DAdt of type_decl
  | DRec of record_decl

let struct_name = function
  | DAdt td -> td.type_name
  | DRec rd -> rd.rec_name

let topo_sort_structs
  (types : type_decl list) (records : record_decl list)
  : struct_decl list =
  let all_names : (string, struct_decl) Hashtbl.t = Hashtbl.create 16 in
  List.iter (fun td -> Hashtbl.replace all_names td.type_name (DAdt td)) types;
  List.iter (fun (rd : record_decl) ->
    Hashtbl.replace all_names rd.rec_name (DRec rd)) records;

  let deps_in_ty acc = function
    | TyInt | TyBool | TyVar _ | TyMeta _ -> acc
    | TyApp (n, []) when Hashtbl.mem all_names n -> n :: acc
    | TyApp _ -> acc
    | TyFun _ -> acc   (* fn pointers don't transmit by-value deps *)
  in
  let deps_of name =
    match Hashtbl.find all_names name with
    | DAdt td ->
        List.fold_left
          (fun acc v -> List.fold_left deps_in_ty acc v.arg_tys)
          [] td.variants
    | DRec rd ->
        List.fold_left
          (fun acc (_, fty) -> deps_in_ty acc fty)
          [] rd.rec_fields
  in

  let visited = Hashtbl.create 16 in
  let order = ref [] in
  let rec visit name =
    if not (Hashtbl.mem visited name) then begin
      Hashtbl.add visited name ();
      List.iter visit (deps_of name);
      order := name :: !order
    end
  in
  List.iter (fun (td : type_decl) -> visit td.type_name) types;
  List.iter (fun (rd : record_decl) -> visit rd.rec_name) records;
  List.rev_map (Hashtbl.find all_names) !order

let emit_adt_definition (td : type_decl) : string =
  let nonempty = List.filter (fun v -> v.arg_tys <> []) td.variants in
  let union_body =
    if nonempty = [] then ""
    else
      let fields = List.map (fun v ->
        let inner = String.concat " "
          (List.mapi (fun i t ->
            Printf.sprintf "%s f%d;" (c_type t) i) v.arg_tys)
        in
        Printf.sprintf "        struct { %s } %s;" inner v.ctor_name)
        nonempty
      in
      "    union {\n"
      ^ String.concat "\n" fields
      ^ "\n    } as;\n"
  in
  Printf.sprintf "struct %s {\n    int tag;\n%s};" td.type_name union_body

let emit_record_forward (rd : record_decl) : string =
  Printf.sprintf "typedef struct %s %s;" rd.rec_name rd.rec_name

let emit_record_definition (rd : record_decl) : string =
  let field_lines =
    List.map (fun (fn, fty) ->
      Printf.sprintf "    %s %s;" (c_type fty) fn) rd.rec_fields
  in
  Printf.sprintf "struct %s {\n%s\n};" rd.rec_name
    (String.concat "\n" field_lines)

let build_ctor_map (types : type_decl list)
  : (string, type_decl * variant * int) Hashtbl.t =
  let h = Hashtbl.create 32 in
  List.iter (fun td ->
    List.iteri (fun i v ->
      Hashtbl.add h v.ctor_name (td, v, i)) td.variants) types;
  h

(* ---------- expression emission ---------- *)

(* Extract the type from any typed expression. After mono, every typed
   node carries its result type. *)
let ty_of_expr : Check.T.expr -> ty = function
  | Check.T.TEInt _ -> TyInt
  | Check.T.TEBool _ -> TyBool
  | Check.T.TEVar (_, t) -> t
  | Check.T.TEFnRef (_, _, t) -> t
  | Check.T.TECall (_, _, t) -> t
  | Check.T.TEBinop (_, _, _, t) -> t
  | Check.T.TEUnop (_, _, t) -> t
  | Check.T.TECtor (_, _, _, t) -> t
  | Check.T.TERecord (_, _, _, t) -> t
  | Check.T.TEField (_, _, t) -> t
  | Check.T.TEIf (_, _, _, t) -> t
  | Check.T.TELet (_, _, _, _, t, _) -> t
  | Check.T.TEMatch (_, _, _, t) -> t
  | Check.T.TEArray (_, _, _, t) -> t
  | Check.T.TEArrayLit (_, _, t) -> t
  | Check.T.TERegion (_, t) -> t
  | Check.T.TEIndex (_, _, t) -> t
  | Check.T.TEAssignIdx (_, _, _, t) -> t
  | Check.T.TELen (_, t) -> t

let rec emit_expr
  (ctor_map : (string, type_decl * variant * int) Hashtbl.t)
  (e : Check.T.expr) : c_code =
  match e with
  | Check.T.TEInt n      -> { stmts = []; value = string_of_int n }
  | Check.T.TEBool true  -> { stmts = []; value = "1" }
  | Check.T.TEBool false -> { stmts = []; value = "0" }
  | Check.T.TEVar (x, _) -> { stmts = []; value = x }

  | Check.T.TEFnRef (name, _, _) ->
      { stmts = []; value = name }

  | Check.T.TECall (callee, args, _) ->
      let cc = emit_expr ctor_map callee in
      let arg_codes = List.map (emit_expr ctor_map) args in
      let stmts =
        cc.stmts @ List.concat_map (fun c -> c.stmts) arg_codes
      in
      let vals = List.map (fun c -> c.value) arg_codes in
      let callee_s = match callee with
        | Check.T.TEVar _ | Check.T.TEFnRef _ -> cc.value
        | _ -> Printf.sprintf "(%s)" cc.value
      in
      let value = Printf.sprintf "%s(%s)" callee_s
        (String.concat ", " vals)
      in
      { stmts; value }

  | Check.T.TEBinop (op, a, b, _) ->
      let ca = emit_expr ctor_map a in
      let cb = emit_expr ctor_map b in
      { stmts = ca.stmts @ cb.stmts;
        value =
          Printf.sprintf "(%s %s %s)" ca.value (c_binop op) cb.value }

  | Check.T.TEUnop (op, e, _) ->
      let ce = emit_expr ctor_map e in
      { stmts = ce.stmts;
        value = Printf.sprintf "(%s%s)" (c_unop op) ce.value }

  | Check.T.TECtor (c, _, args, result_ty) ->
      let arg_codes = List.map (emit_expr ctor_map) args in
      let stmts = List.concat_map (fun co -> co.stmts) arg_codes in
      let vals  = List.map (fun co -> co.value) arg_codes in
      let (_, _, tag) =
        try Hashtbl.find ctor_map c
        with Not_found ->
          failwith (Printf.sprintf "emit: unknown ctor %S" c)
      in
      let owner_c = c_type result_ty in
      let value =
        if vals = [] then
          Printf.sprintf "((%s){ .tag = %d })" owner_c tag
        else
          let inits = String.concat ", "
            (List.mapi (fun i v ->
              Printf.sprintf ".f%d = %s" i v) vals)
          in
          Printf.sprintf "((%s){ .tag = %d, .as = { .%s = { %s } } })"
            owner_c tag c inits
      in
      { stmts; value }

  | Check.T.TERecord (_name, _, fields, result_ty) ->
      let field_codes =
        List.map (fun (fn, e) -> (fn, emit_expr ctor_map e)) fields
      in
      let stmts =
        List.concat_map (fun (_, c) -> c.stmts) field_codes
      in
      let inits = String.concat ", "
        (List.map (fun (fn, c) ->
          Printf.sprintf ".%s = %s" fn c.value) field_codes)
      in
      let value =
        Printf.sprintf "((%s){ %s })" (c_type result_ty) inits
      in
      { stmts; value }

  | Check.T.TEField (e, fname, _) ->
      let ce = emit_expr ctor_map e in
      let value = match e with
        | Check.T.TEVar _ | Check.T.TEFnRef _ ->
            Printf.sprintf "%s.%s" ce.value fname
        | _ ->
            Printf.sprintf "(%s).%s" ce.value fname
      in
      { stmts = ce.stmts; value }

  | Check.T.TEIf (cond, then_b, else_b, result_ty) ->
      let cc = emit_expr ctor_map cond in
      let ct = emit_expr ctor_map then_b in
      let ce = emit_expr ctor_map else_b in
      let tmp = fresh "tmp" in
      let indent ss = List.map (fun s -> "    " ^ s) ss in
      let stmts =
        cc.stmts
        @ [Printf.sprintf "%s %s;" (c_type result_ty) tmp]
        @ [Printf.sprintf "if (%s) {" cc.value]
        @ indent ct.stmts
        @ [Printf.sprintf "    %s = %s;" tmp ct.value]
        @ ["} else {"]
        @ indent ce.stmts
        @ [Printf.sprintf "    %s = %s;" tmp ce.value]
        @ ["}"]
      in
      { stmts; value = tmp }

  | Check.T.TELet (x, vt, value_e, body, body_ty, auto_drop) ->
      let cv = emit_expr ctor_map value_e in
      let cb = emit_expr ctor_map body in
      let decl =
        if x = "_" then
          Printf.sprintf "(void)(%s);" cv.value
        else
          Printf.sprintf "%s %s = %s;" (c_type vt) x cv.value
      in
      if auto_drop then begin
        (* Materialise the body result into a temp, then free x's cell,
           then yield the temp. This way:
             - body sees x alive while it's being evaluated
             - the cell is released before we leave this let
             - the result value (which is independent — it's a copy or
               an Own from elsewhere) survives the release *)
        let temp = fresh "_let_result" in
        let body_decl =
          Printf.sprintf "%s %s = %s;" (c_type body_ty) temp cb.value
        in
        let free_stmt =
          match vt with
          | TyApp ("Region", _) ->
              (* Free the region's buffer and bump its generation.
                 The header itself stays alive — dangling Array
                 handles use it for gen checks. *)
              Printf.sprintf
                "if (%s.header->gen == %s.expected_gen) { \
                 free(%s.header->buffer); %s.header->buffer = NULL; \
                 %s.header->buffer_size = 0; %s.header->used = 0; \
                 %s.header->gen++; }"
                x x x x x x x
          | _ -> failwith "emit: auto_drop on non-linear type"
        in
        let stmts =
          cv.stmts
          @ [decl]
          @ cb.stmts
          @ [body_decl; free_stmt]
        in
        { stmts; value = temp }
      end else begin
        let stmts =
          cv.stmts
          @ [decl]
          @ cb.stmts
        in
        { stmts; value = cb.value }
      end

  | Check.T.TEMatch (scrut, scrut_ty, arms, result_ty) ->
      let cs = emit_expr ctor_map scrut in
      let scrut_var  = fresh "scrut" in
      let result_var = fresh "match_result" in
      let scrut_decl =
        Printf.sprintf "%s %s = %s;" (c_type scrut_ty) scrut_var cs.value
      in
      let result_decl =
        Printf.sprintf "%s %s;" (c_type result_ty) result_var
      in
      let emit_arm (pat, body) =
        let bindings = match pat with
          | PWild -> []
          | PCtor (c, vs) ->
              let (_, v, _) = Hashtbl.find ctor_map c in
              List.filter_map (fun ((var, t), i) ->
                if var = "_" then None
                else
                  Some (Printf.sprintf
                    "    %s %s = %s.as.%s.f%d;"
                    (c_type t) var scrut_var c i))
              (List.mapi (fun i x -> (x, i))
                (List.combine vs v.arg_tys))
        in
        let cb = emit_expr ctor_map body in
        let body_lines =
          (List.map (fun s -> "    " ^ s) cb.stmts)
          @ [Printf.sprintf "    %s = %s;" result_var cb.value]
          @ ["    break;"]
        in
        match pat with
        | PWild ->
            ["default: {"] @ bindings @ body_lines @ ["}"]
        | PCtor (c, _) ->
            let (_, _, tag) = Hashtbl.find ctor_map c in
            [Printf.sprintf "case %d: { /* %s */" tag c]
            @ bindings @ body_lines @ ["}"]
      in
      let arm_blocks = List.concat_map emit_arm arms in
      let has_wild = List.exists (fun (p, _) -> p = PWild) arms in
      let trailing =
        if has_wild then []
        else ["default: abort();"]
      in
      let switch_lines =
        [Printf.sprintf "switch (%s.tag) {" scrut_var]
        @ (List.map (fun s -> "    " ^ s) arm_blocks)
        @ (List.map (fun s -> "    " ^ s) trailing)
        @ ["}"]
      in
      let stmts =
        cs.stmts
        @ [scrut_decl; result_decl]
        @ switch_lines
      in
      { stmts; value = result_var }

  | Check.T.TEArray (region_e, size_e, init_e, result_ty) ->
      (* Bump-allocate N*sizeof(T) inside the region's buffer. Returns
         a handle {region, offset, len, expected_gen}. The handle is
         copyable; the buffer is owned by the region. *)
      let cr = emit_expr ctor_map region_e in
      let cn = emit_expr ctor_map size_e in
      let cv = emit_expr ctor_map init_e in
      let r_var = fresh "_r" in
      let n_var = fresh "_n" in
      let off_var = fresh "_off" in
      let slots_var = fresh "_slots" in
      let i_var = fresh "_i" in
      let arr_var = fresh "_arr" in
      let arr_c = c_type result_ty in
      let elem_c = c_type (ty_of_expr init_e) in
      let stmts = cr.stmts @ cn.stmts @ cv.stmts @ [
        Printf.sprintf "Region %s = %s;" r_var cr.value;
        Printf.sprintf "if (%s.header->gen != %s.expected_gen) abort();"
          r_var r_var;
        Printf.sprintf "int %s = %s;" n_var cn.value;
        Printf.sprintf "if (%s < 0) abort();" n_var;
        Printf.sprintf
          "if (%s.header->used + (size_t)%s * sizeof(%s) > %s.header->buffer_size) abort();"
          r_var n_var elem_c r_var;
        Printf.sprintf "int %s = (int)%s.header->used;" off_var r_var;
        Printf.sprintf "%s.header->used += (size_t)%s * sizeof(%s);"
          r_var n_var elem_c;
        Printf.sprintf "%s* %s = (%s*)(%s.header->buffer + %s);"
          elem_c slots_var elem_c r_var off_var;
        Printf.sprintf "for (int %s = 0; %s < %s; %s++) %s[%s] = %s;"
          i_var i_var n_var i_var slots_var i_var cv.value;
        Printf.sprintf
          "%s %s = ((%s){ .region = %s.header, .offset = %s, .len = %s, .expected_gen = %s.expected_gen });"
          arr_c arr_var arr_c r_var off_var n_var r_var;
      ] in
      { stmts; value = arr_var }

  | Check.T.TEArrayLit (region_e, elems, result_ty) ->
      (* array(r, [v0..vN-1]): bump-allocate N slots in r, store the
         literal values in order. Same shape as TEArray but each slot
         gets its own value instead of a single fill. *)
      let cr = emit_expr ctor_map region_e in
      let elem_codes = List.map (emit_expr ctor_map) elems in
      let r_var = fresh "_r" in
      let off_var = fresh "_off" in
      let slots_var = fresh "_slots" in
      let arr_var = fresh "_arr" in
      let arr_c = c_type result_ty in
      let elem_ty = match result_ty with
        | TyApp ("Array", [inner]) -> inner
        | _ -> failwith "emit TEArrayLit: result not Array[_]"
      in
      let elem_c = c_type elem_ty in
      let n = List.length elems in
      let init_stmts = List.mapi (fun i c ->
        Printf.sprintf "%s[%d] = %s;" slots_var i c.value) elem_codes
      in
      let stmts = cr.stmts
        @ List.concat_map (fun c -> c.stmts) elem_codes
        @ [
          Printf.sprintf "Region %s = %s;" r_var cr.value;
          Printf.sprintf "if (%s.header->gen != %s.expected_gen) abort();"
            r_var r_var;
          Printf.sprintf
            "if (%s.header->used + (size_t)%d * sizeof(%s) > %s.header->buffer_size) abort();"
            r_var n elem_c r_var;
          Printf.sprintf "int %s = (int)%s.header->used;" off_var r_var;
          Printf.sprintf "%s.header->used += (size_t)%d * sizeof(%s);"
            r_var n elem_c;
          Printf.sprintf "%s* %s = (%s*)(%s.header->buffer + %s);"
            elem_c slots_var elem_c r_var off_var;
        ] @ init_stmts @ [
          Printf.sprintf
            "%s %s = ((%s){ .region = %s.header, .offset = %s, .len = %d, .expected_gen = %s.expected_gen });"
            arr_c arr_var arr_c r_var off_var n r_var;
        ]
      in
      { stmts; value = arr_var }

  | Check.T.TERegion (size_e, _) ->
      (* Allocate a Region_header on the heap, then its buffer of N
         bytes. The header outlives the buffer so dangling Array
         handles can detect death via gen mismatch. *)
      let cn = emit_expr ctor_map size_e in
      let n_var = fresh "_n" in
      let h_var = fresh "_h" in
      let r_var = fresh "_reg" in
      let stmts = cn.stmts @ [
        Printf.sprintf "int %s = %s;" n_var cn.value;
        Printf.sprintf "if (%s < 0) abort();" n_var;
        Printf.sprintf
          "struct Region_header* %s = malloc(sizeof(struct Region_header));"
          h_var;
        Printf.sprintf "if (!%s) abort();" h_var;
        Printf.sprintf "%s->gen = 1;" h_var;
        Printf.sprintf "%s->buffer = malloc((size_t)%s);" h_var n_var;
        Printf.sprintf "if (!%s->buffer) abort();" h_var;
        Printf.sprintf "%s->buffer_size = (size_t)%s;" h_var n_var;
        Printf.sprintf "%s->used = 0;" h_var;
        Printf.sprintf
          "Region %s = ((Region){ .header = %s, .expected_gen = 1 });"
          r_var h_var;
      ] in
      { stmts; value = r_var }

  | Check.T.TEIndex (arr_e, idx_e, elem_ty) ->
      let ca = emit_expr ctor_map arr_e in
      let ci = emit_expr ctor_map idx_e in
      let a_var = fresh "_a" in
      let i_var = fresh "_i" in
      let result_var = fresh "_idx" in
      let arr_c = c_type (ty_of_expr arr_e) in
      let elem_c = c_type elem_ty in
      let stmts = ca.stmts @ ci.stmts @ [
        Printf.sprintf "%s %s = %s;" arr_c a_var ca.value;
        Printf.sprintf "int %s = %s;" i_var ci.value;
        Printf.sprintf "if (%s.region->gen != %s.expected_gen) abort();"
          a_var a_var;
        Printf.sprintf "if (%s < 0 || %s >= %s.len) abort();"
          i_var i_var a_var;
        Printf.sprintf
          "%s %s = ((%s*)(%s.region->buffer + %s.offset))[%s];"
          elem_c result_var elem_c a_var a_var i_var;
      ] in
      { stmts; value = result_var }

  | Check.T.TEAssignIdx (arr_e, idx_e, val_e, _) ->
      let ca = emit_expr ctor_map arr_e in
      let ci = emit_expr ctor_map idx_e in
      let cv = emit_expr ctor_map val_e in
      let a_var = fresh "_a" in
      let i_var = fresh "_i" in
      let arr_c = c_type (ty_of_expr arr_e) in
      let elem_c = c_type (ty_of_expr val_e) in
      let stmts = ca.stmts @ ci.stmts @ cv.stmts @ [
        Printf.sprintf "%s %s = %s;" arr_c a_var ca.value;
        Printf.sprintf "int %s = %s;" i_var ci.value;
        Printf.sprintf "if (%s.region->gen != %s.expected_gen) abort();"
          a_var a_var;
        Printf.sprintf "if (%s < 0 || %s >= %s.len) abort();"
          i_var i_var a_var;
        Printf.sprintf
          "((%s*)(%s.region->buffer + %s.offset))[%s] = %s;"
          elem_c a_var a_var i_var cv.value;
      ] in
      { stmts; value = "0" }

  | Check.T.TELen (arr_e, _) ->
      let ca = emit_expr ctor_map arr_e in
      let value = match arr_e with
        | Check.T.TEVar _ | Check.T.TEFnRef _ ->
            Printf.sprintf "%s.len" ca.value
        | _ -> Printf.sprintf "(%s).len" ca.value
      in
      { stmts = ca.stmts; value }

(* ---------- function emission ---------- *)

let emit_extern_decl (e : Check.T.extern) : string =
  let params_s =
    if e.params = [] then "void"
    else
      String.concat ", "
        (List.map (fun (x, t) ->
          Printf.sprintf "%s %s" (c_type t) x) e.params)
  in
  Printf.sprintf "extern %s %s(%s);" (c_type e.return_ty) e.name params_s

let emit_func_decl (f : Check.T.func) : string =
  let params_s =
    if f.params = [] then "void"
    else
      String.concat ", "
        (List.map (fun (x, t) ->
          Printf.sprintf "%s %s" (c_type t) x) f.params)
  in
  Printf.sprintf "%s %s(%s);" (c_type f.return_ty) f.name params_s

let emit_func_def ctor_map (f : Check.T.func) : string =
  reset_counter ();
  let params_s =
    if f.params = [] then "void"
    else
      String.concat ", "
        (List.map (fun (x, t) ->
          Printf.sprintf "%s %s" (c_type t) x) f.params)
  in
  let cb = emit_expr ctor_map f.body in
  let body_lines =
    cb.stmts @ [Printf.sprintf "return %s;" cb.value]
  in
  let indented = List.map (fun s -> "    " ^ s) body_lines in
  Printf.sprintf "%s %s(%s) {\n%s\n}"
    (c_type f.return_ty) f.name params_s
    (String.concat "\n" indented)

(* ---------- whole program ---------- *)

let emit (prog : Check.T.program) : string =
  let prog =
    { prog with funcs = List.map alpha_rename_func prog.funcs }
  in
  collect_program prog;
  let ctor_map = build_ctor_map prog.types in
  let adt_forwards = List.map emit_adt_forward prog.types in
  let rec_forwards = List.map emit_record_forward prog.records in
  let array_forwards = emit_array_forwards () in
  let fn_typedefs  = emit_fn_typedefs () in
  let ordered_structs = topo_sort_structs prog.types prog.records in
  let struct_defs = List.map (function
    | DAdt td -> emit_adt_definition td
    | DRec rd -> emit_record_definition rd) ordered_structs in
  let extern_decls = List.map emit_extern_decl prog.externs in
  let decls        = List.map emit_func_decl prog.funcs in
  let defs         = List.map (emit_func_def ctor_map) prog.funcs in
  let header =
    "/* generated by orto */\n\
     #include <stdlib.h>\n\
     #include <stddef.h>\n\
     \n\
     struct Region_header { \
     int gen; char* buffer; size_t buffer_size; size_t used; };\n\
     typedef struct { struct Region_header* header; int expected_gen; } Region;"
  in
  String.concat "\n\n"
    ([header]
     @ adt_forwards
     @ rec_forwards
     @ array_forwards
     @ fn_typedefs
     @ struct_defs
     @ extern_decls
     @ decls
     @ defs)

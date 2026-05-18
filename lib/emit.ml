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

(* String literal pool. Every "..." in the program is deduplicated and
   assigned a byte-offset into one shared static buffer. The buffer
   sits at region slot 0 (reserved at startup, never freed). Each
   literal emits to an Array[byte] handle with .slot=0, the offset,
   length, and the static gen=1. *)
let string_pool : (string, int) Hashtbl.t = Hashtbl.create 16
let string_pool_order : (string * int) list ref = ref []
let string_pool_size = ref 0

let register_string (s : string) : int =
  match Hashtbl.find_opt string_pool s with
  | Some off -> off
  | None ->
      let off = !string_pool_size in
      Hashtbl.replace string_pool s off;
      string_pool_order := (s, off) :: !string_pool_order;
      string_pool_size := off + String.length s;
      off

let mangle_array_name (inner : ty) : string =
  "Array_" ^ Mono.mangle_ty inner

let register_array (mangled : string) (inner : ty) =
  if not (Hashtbl.mem array_types_seen mangled) then begin
    Hashtbl.add array_types_seen mangled ();
    array_types_order := (mangled, inner) :: !array_types_order
  end

let rec collect_ty (t : ty) : unit =
  match t with
  | TyInt | TyBool -> ()
  | TyVar n ->
      failwith (Printf.sprintf "emit collect_ty: TyVar %S after mono" n)
  | TyApp ("Array", [inner]) ->
      collect_ty inner;
      register_array (mangle_array_name inner) inner
  | TyApp ("Array", _) ->
      failwith "emit collect_ty: Array with wrong arity"
  | TyApp ("Region", []) -> ()
      (* Region runtime is emitted unconditionally at the top of the file. *)
  | TyApp ("Region", _) ->
      failwith "emit collect_ty: Region takes no type arguments"
  | TyApp ("byte", []) -> ()
      (* byte is a primitive; maps directly to uint8_t in C. *)
  | TyApp ("byte", _) ->
      failwith "emit collect_ty: byte takes no type arguments"
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
  | TyPtr inner -> collect_ty inner
  | TyMeta _ -> failwith "emit collect_ty: TyMeta"

let rec collect_expr (e : Check.T.expr) : unit =
  match e with
  | Check.T.TEInt _ | Check.T.TEBool _ -> ()
  | Check.T.TEStringLit s ->
      let _ = register_string s in
      (* String literal materialises as an Array[byte] handle — make
         sure the Array_byte typedef is emitted. *)
      collect_ty (TyApp ("Array", [TyApp ("byte", [])]))
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
  | Check.T.TEStackRegion (n, t) -> collect_expr n; collect_ty t
  | Check.T.TEAlignedRegion (n, a, t) ->
      collect_expr n; collect_expr a; collect_ty t
  | Check.T.TEIndex (a, i, t) ->
      collect_expr a; collect_expr i; collect_ty t
  | Check.T.TEAssignIdx (a, i, v, t) ->
      collect_expr a; collect_expr i; collect_expr v; collect_ty t
  | Check.T.TELen (e, t) -> collect_expr e; collect_ty t
  | Check.T.TESlice (a, lo, hi, t) ->
      collect_expr a; collect_expr lo; collect_expr hi; collect_ty t
  | Check.T.TEToInt e  -> collect_expr e
  | Check.T.TEToByte e -> collect_expr e
  | Check.T.TECAlloc (et, n, t) -> collect_ty et; collect_expr n; collect_ty t
  | Check.T.TECFree p -> collect_expr p
  | Check.T.TENullPtr t -> collect_ty t
  | Check.T.TEIsNull p -> collect_expr p
  | Check.T.TEArrayData (a, t) -> collect_expr a; collect_ty t
  | Check.T.TEDeref (p, t) -> collect_expr p; collect_ty t
  | Check.T.TEAssign (_, v, t) -> collect_expr v; collect_ty t
  | Check.T.TEWhile (c, b) -> collect_expr c; collect_expr b
  | Check.T.TEBreak | Check.T.TEContinue -> ()
  | Check.T.TEReturn (v, t) -> collect_expr v; collect_ty t
  | Check.T.TETryAt (a, i, t) -> collect_expr a; collect_expr i; collect_ty t
  | Check.T.TEDrop (e, t) -> collect_expr e; collect_ty t

let collect_program (prog : Check.T.program) : unit =
  Hashtbl.clear fn_types_seen;
  fn_types_order := [];
  Hashtbl.clear array_types_seen;
  array_types_order := [];
  Hashtbl.clear string_pool;
  string_pool_order := [];
  string_pool_size := 0;
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

let rec c_type (t : ty) : string =
  match t with
  | TyInt -> "int"
  | TyBool -> "int"
  | TyApp ("byte", []) -> "uint8_t"
  | TyApp ("Array", [inner]) -> mangle_array_name inner
  | TyApp ("Region", []) -> "Region"
  | TyApp (n, []) -> n
  | TyFun _ -> Mono.mangle_ty t
  | TyPtr inner -> c_type inner ^ "*"
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

(* One typedef per distinct Array[T] element type. The handle carries
   the region slot, byte-offset of this slice within the region's
   block, length, and the slot's expected generation. *)
let emit_array_forwards () : string list =
  List.rev_map (fun (mangled, _inner) ->
    Printf.sprintf
      "typedef struct { int slot; int offset; int len; int expected_gen; } %s;"
      mangled)
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
    | TEInt _ | TEBool _ | TEStringLit _ | TEFnRef _ -> e
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
            | POr _ -> (p, rn env body)
            | PInt _ | PBool _ | PStr _ -> (p, rn env body)
            | PBind x when x = "_" -> (p, rn env body)
            | PBind x ->
                let x' = fresh x in
                let env' = (x, x') :: env in
                (PBind x', rn env' body)
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
    | TEStackRegion (n, t) -> TEStackRegion (rn env n, t)
    | TEAlignedRegion (n, a, t) ->
        TEAlignedRegion (rn env n, rn env a, t)
    | TEIndex (a, i, t) -> TEIndex (rn env a, rn env i, t)
    | TEAssignIdx (a, i, v, t) ->
        TEAssignIdx (rn env a, rn env i, rn env v, t)
    | TELen (e, t) -> TELen (rn env e, t)
    | TESlice (a, lo, hi, t) ->
        TESlice (rn env a, rn env lo, rn env hi, t)
    | TEToInt e  -> TEToInt (rn env e)
    | TEToByte e -> TEToByte (rn env e)
    | TECAlloc (et, n, t) -> TECAlloc (et, rn env n, t)
    | TECFree p -> TECFree (rn env p)
    | TENullPtr t -> TENullPtr t
    | TEIsNull p -> TEIsNull (rn env p)
    | TEArrayData (a, t) -> TEArrayData (rn env a, t)
    | TEDeref (p, t) -> TEDeref (rn env p, t)
    | TEAssign (x, v, t) ->
        let x' = try List.assoc x env with Not_found -> x in
        TEAssign (x', rn env v, t)
    | TEWhile (c, b) -> TEWhile (rn env c, rn env b)
    | TEBreak | TEContinue -> e
    | TEReturn (v, t) -> TEReturn (rn env v, t)
    | TETryAt (a, i, t) -> TETryAt (rn env a, rn env i, t)
    | TEDrop (e, t) -> TEDrop (rn env e, t)
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
    | TyPtr _ -> acc   (* raw pointers don't transmit by-value deps either *)
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
  | Check.T.TEStringLit _ -> TyApp ("Array", [TyApp ("byte", [])])
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
  | Check.T.TEStackRegion (_, t) -> t
  | Check.T.TEAlignedRegion (_, _, t) -> t
  | Check.T.TEIndex (_, _, t) -> t
  | Check.T.TEAssignIdx (_, _, _, t) -> t
  | Check.T.TELen (_, t) -> t
  | Check.T.TESlice (_, _, _, t) -> t
  | Check.T.TEToInt _  -> TyInt
  | Check.T.TEToByte _ -> TyApp ("byte", [])
  | Check.T.TECAlloc (_, _, t) -> t
  | Check.T.TECFree _ -> TyInt
  | Check.T.TENullPtr t -> t
  | Check.T.TEIsNull _ -> TyBool
  | Check.T.TEArrayData (_, t) -> t
  | Check.T.TEDeref (_, t) -> t
  | Check.T.TEAssign (_, _, _) -> TyInt
  | Check.T.TEWhile (_, _) -> TyInt
  | Check.T.TEBreak | Check.T.TEContinue -> TyInt
  | Check.T.TEReturn (_, _) -> TyInt
  | Check.T.TETryAt (_, _, t) -> t
  | Check.T.TEDrop (_, _) -> TyInt

(* Release a Region's buffer (if it's heap-allocated), bump the
   generation, and push the slot back onto the free list. Stack
   regions skip the free — their storage is reclaimed when the
   surrounding C function returns. Used by let-scope auto_drop and
   function-end param drops. *)
let is_catchall_pat_emit = function
  | PWild | PBind _ -> true
  | _ -> false

(* Emit a call to the right drop function for a linear type. After mono,
   the type name carries its module mangling (`net__Socket`); the helper
   in check.ml derives the matching drop fn name. For Region the
   runtime supplies `drop_Region` directly. *)
let drop_call_stmt (var_name : string) (t : ty) : string =
  match t with
  | TyApp (n, _) ->
      let fn = Check.drop_fn_name_for n in
      Printf.sprintf "%s(%s);" fn var_name
  | _ ->
      failwith
        (Printf.sprintf "emit: drop on non-TyApp type %s" (Ast.show_ty t))

let rec emit_expr
  (ctor_map : (string, type_decl * variant * int) Hashtbl.t)
  (e : Check.T.expr) : c_code =
  match e with
  | Check.T.TEInt n      -> { stmts = []; value = string_of_int n }
  | Check.T.TEBool true  -> { stmts = []; value = "1" }
  | Check.T.TEBool false -> { stmts = []; value = "0" }
  | Check.T.TEStringLit s ->
      let off = register_string s in
      let len = String.length s in
      let value = Printf.sprintf
        "((Array_byte){ .slot = 0, .offset = %d, .len = %d, .expected_gen = 1 })"
        off len
      in
      { stmts = []; value }
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
        (* Materialise the body result into a temp, then free x's
           value, then yield the temp. The body sees x alive while
           it's being evaluated; the value is released before control
           leaves this let. *)
        let temp = fresh "_let_result" in
        let body_decl =
          Printf.sprintf "%s %s = %s;" (c_type body_ty) temp cb.value
        in
        let free_stmt = drop_call_stmt x vt in
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
      (* Dispatch by scrutinee shape. After mono, an ADT shows up as
         TyApp(name, []) where name is in the ADT environment, i.e.
         present in ctor_map under at least one ctor name. We detect
         the four non-ADT scrutinee shapes by structural type. *)
      let is_adt_scrut =
        match scrut_ty with
        | TyInt | TyBool -> false
        | TyApp ("byte", []) -> false
        | TyApp ("Array", _) -> false
        | TyApp _ -> true
        | _ -> true
      in
      if is_adt_scrut then begin
        let emit_arm (pat, body) =
          let bindings = match pat with
            | PWild -> []
            | POr _ -> []
            | PBind x when x <> "_" ->
                [Printf.sprintf "    %s %s = %s;" (c_type scrut_ty) x scrut_var]
            | PBind _ -> []
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
            | _ -> []
          in
          let cb = emit_expr ctor_map body in
          let body_lines =
            (List.map (fun s -> "    " ^ s) cb.stmts)
            @ [Printf.sprintf "    %s = %s;" result_var cb.value]
            @ ["    break;"]
          in
          match pat with
          | PWild | PBind _ ->
              ["default: {"] @ bindings @ body_lines @ ["}"]
          | PCtor (c, _) ->
              let (_, _, tag) = Hashtbl.find ctor_map c in
              [Printf.sprintf "case %d: { /* %s */" tag c]
              @ bindings @ body_lines @ ["}"]
          | POr pats ->
              let labels = List.map (function
                | PCtor (c, _) ->
                    let (_, _, tag) = Hashtbl.find ctor_map c in
                    Printf.sprintf "case %d: /* %s */" tag c
                | _ -> failwith "emit: malformed ADT or-pattern") pats
              in
              labels @ ["{"] @ bindings @ body_lines @ ["}"]
          | _ -> failwith "emit: literal pattern in ADT match"
        in
        let arm_blocks = List.concat_map emit_arm arms in
        let has_catchall = List.exists (fun (p, _) ->
          match p with PWild | PBind _ -> true | _ -> false) arms in
        let trailing =
          if has_catchall then []
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
      end else begin
        (* Non-ADT scrutinee: int, bool, byte, or Array[byte]. Emit a
           chain of `if (cond) { ... } else if (cond) { ... } ... else
           { /* catch-all */ }`. *)
        let pat_test pat =
          let rec single = function
            | PInt n  -> Printf.sprintf "%s == %d" scrut_var n
            | PBool b -> Printf.sprintf "%s == %d" scrut_var (if b then 1 else 0)
            | PStr s ->
                (* Compare by length first, then bytes. We register
                   the string in the pool so we can memcmp against the
                   static buffer at a known offset. *)
                let off = register_string s in
                let len = String.length s in
                Printf.sprintf
                  "(%s).len == %d && memcmp(ORTO_REGIONS[(%s).slot].buffer + (%s).offset, ORTO_STATIC_BYTES + %d, %d) == 0"
                  scrut_var len scrut_var scrut_var off len
            | POr ps -> String.concat " || " (List.map single ps)
            | PWild | PBind _ -> "1"
            | PCtor _ -> failwith "emit: ctor pattern in non-ADT match"
          in
          single pat
        in
        let emit_arm idx (pat, body) =
          let cb = emit_expr ctor_map body in
          let is_catchall = is_catchall_pat_emit pat in
          let bind_stmt = match pat with
            | PBind x when x <> "_" ->
                [Printf.sprintf "    %s %s = %s;" (c_type scrut_ty) x scrut_var]
            | _ -> []
          in
          let body_lines =
            bind_stmt
            @ (List.map (fun s -> "    " ^ s) cb.stmts)
            @ [Printf.sprintf "    %s = %s;" result_var cb.value]
          in
          let head =
            if idx = 0 then
              if is_catchall then "if (1) {"
              else Printf.sprintf "if (%s) {" (pat_test pat)
            else if is_catchall then "else {"
            else Printf.sprintf "else if (%s) {" (pat_test pat)
          in
          [head] @ body_lines @ ["}"]
        in
        let chain = List.concat (List.mapi emit_arm arms) in
        let has_catchall = List.exists (fun (p, _) -> is_catchall_pat_emit p) arms in
        let safety =
          if has_catchall then []
          else
            (* Should be unreachable for bool that covers both; for
               int/byte/bytes check.ml requires a catch-all so we never
               reach here. Defensive abort just in case. *)
            ["else { abort(); }"]
        in
        let stmts =
          cs.stmts
          @ [scrut_decl; result_decl]
          @ chain
          @ safety
        in
        { stmts; value = result_var }
      end

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
        Printf.sprintf "if (ORTO_REGIONS[%s.slot].gen != %s.expected_gen) abort();"
          r_var r_var;
        Printf.sprintf "int %s = %s;" n_var cn.value;
        Printf.sprintf "if (%s < 0) abort();" n_var;
        Printf.sprintf
          "if (ORTO_REGIONS[%s.slot].used + (size_t)%s * sizeof(%s) > ORTO_REGIONS[%s.slot].buffer_size) abort();"
          r_var n_var elem_c r_var;
        Printf.sprintf "int %s = (int)ORTO_REGIONS[%s.slot].used;"
          off_var r_var;
        Printf.sprintf "ORTO_REGIONS[%s.slot].used += (size_t)%s * sizeof(%s);"
          r_var n_var elem_c;
        Printf.sprintf "%s* %s = (%s*)(ORTO_REGIONS[%s.slot].buffer + %s);"
          elem_c slots_var elem_c r_var off_var;
        Printf.sprintf "for (int %s = 0; %s < %s; %s++) %s[%s] = %s;"
          i_var i_var n_var i_var slots_var i_var cv.value;
        Printf.sprintf
          "%s %s = ((%s){ .slot = %s.slot, .offset = %s, .len = %s, .expected_gen = %s.expected_gen });"
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
          Printf.sprintf
            "if (ORTO_REGIONS[%s.slot].gen != %s.expected_gen) abort();"
            r_var r_var;
          Printf.sprintf
            "if (ORTO_REGIONS[%s.slot].used + (size_t)%d * sizeof(%s) > ORTO_REGIONS[%s.slot].buffer_size) abort();"
            r_var n elem_c r_var;
          Printf.sprintf "int %s = (int)ORTO_REGIONS[%s.slot].used;"
            off_var r_var;
          Printf.sprintf "ORTO_REGIONS[%s.slot].used += (size_t)%d * sizeof(%s);"
            r_var n elem_c;
          Printf.sprintf "%s* %s = (%s*)(ORTO_REGIONS[%s.slot].buffer + %s);"
            elem_c slots_var elem_c r_var off_var;
        ] @ init_stmts @ [
          Printf.sprintf
            "%s %s = ((%s){ .slot = %s.slot, .offset = %s, .len = %d, .expected_gen = %s.expected_gen });"
            arr_c arr_var arr_c r_var off_var n r_var;
        ]
      in
      { stmts; value = arr_var }

  | Check.T.TERegion (size_e, _) ->
      (* Take a slot from the slab, malloc its buffer. *)
      let cn = emit_expr ctor_map size_e in
      let n_var = fresh "_n" in
      let slot_var = fresh "_slot" in
      let r_var = fresh "_reg" in
      let stmts = cn.stmts @ [
        Printf.sprintf "int %s = %s;" n_var cn.value;
        Printf.sprintf "if (%s < 0) abort();" n_var;
        Printf.sprintf "if (ORTO_REGION_FREE_HEAD < 0) abort();";
        Printf.sprintf "int %s = ORTO_REGION_FREE_HEAD;" slot_var;
        Printf.sprintf
          "ORTO_REGION_FREE_HEAD = ORTO_REGIONS[%s].next_free;" slot_var;
        Printf.sprintf
          "ORTO_REGIONS[%s].buffer = malloc((size_t)%s);" slot_var n_var;
        Printf.sprintf "if (!ORTO_REGIONS[%s].buffer) abort();" slot_var;
        Printf.sprintf "ORTO_REGIONS[%s].buffer_size = (size_t)%s;"
          slot_var n_var;
        Printf.sprintf "ORTO_REGIONS[%s].used = 0;" slot_var;
        Printf.sprintf "ORTO_REGIONS[%s].next_free = -1;" slot_var;
        Printf.sprintf "ORTO_REGIONS[%s].is_stack = 0;" slot_var;
        Printf.sprintf
          "Region %s = ((Region){ .slot = %s, .expected_gen = ORTO_REGIONS[%s].gen });"
          r_var slot_var slot_var;
      ] in
      { stmts; value = r_var }

  | Check.T.TEStackRegion (size_e, _) ->
      (* Allocate a local C array of N bytes (N is a literal) and wire
         it into a slab slot. is_stack=1 so drop skips free. *)
      let n_literal = match size_e with
        | Check.T.TEInt n -> n
        | _ -> failwith "emit TEStackRegion: size not an int literal"
      in
      let stor_var = fresh "_stack_buf" in
      let slot_var = fresh "_slot" in
      let r_var = fresh "_reg" in
      let stmts = [
        Printf.sprintf "char %s[%d];" stor_var n_literal;
        Printf.sprintf "if (ORTO_REGION_FREE_HEAD < 0) abort();";
        Printf.sprintf "int %s = ORTO_REGION_FREE_HEAD;" slot_var;
        Printf.sprintf
          "ORTO_REGION_FREE_HEAD = ORTO_REGIONS[%s].next_free;" slot_var;
        Printf.sprintf "ORTO_REGIONS[%s].buffer = %s;" slot_var stor_var;
        Printf.sprintf "ORTO_REGIONS[%s].buffer_size = %d;"
          slot_var n_literal;
        Printf.sprintf "ORTO_REGIONS[%s].used = 0;" slot_var;
        Printf.sprintf "ORTO_REGIONS[%s].next_free = -1;" slot_var;
        Printf.sprintf "ORTO_REGIONS[%s].is_stack = 1;" slot_var;
        Printf.sprintf
          "Region %s = ((Region){ .slot = %s, .expected_gen = ORTO_REGIONS[%s].gen });"
          r_var slot_var slot_var;
      ] in
      { stmts; value = r_var }

  | Check.T.TEAlignedRegion (size_e, align_e, _) ->
      (* posix_memalign for the buffer; same slab dance otherwise. *)
      let cn = emit_expr ctor_map size_e in
      let ca = emit_expr ctor_map align_e in
      let n_var = fresh "_n" in
      let a_var = fresh "_a" in
      let slot_var = fresh "_slot" in
      let buf_var = fresh "_buf" in
      let r_var = fresh "_reg" in
      let stmts = cn.stmts @ ca.stmts @ [
        Printf.sprintf "int %s = %s;" n_var cn.value;
        Printf.sprintf "if (%s < 0) abort();" n_var;
        Printf.sprintf "int %s = %s;" a_var ca.value;
        Printf.sprintf "if (ORTO_REGION_FREE_HEAD < 0) abort();";
        Printf.sprintf "int %s = ORTO_REGION_FREE_HEAD;" slot_var;
        Printf.sprintf
          "ORTO_REGION_FREE_HEAD = ORTO_REGIONS[%s].next_free;" slot_var;
        Printf.sprintf "void* %s = NULL;" buf_var;
        Printf.sprintf
          "if (posix_memalign(&%s, (size_t)%s, (size_t)%s) != 0) abort();"
          buf_var a_var n_var;
        Printf.sprintf "ORTO_REGIONS[%s].buffer = (char*)%s;"
          slot_var buf_var;
        Printf.sprintf "ORTO_REGIONS[%s].buffer_size = (size_t)%s;"
          slot_var n_var;
        Printf.sprintf "ORTO_REGIONS[%s].used = 0;" slot_var;
        Printf.sprintf "ORTO_REGIONS[%s].next_free = -1;" slot_var;
        Printf.sprintf "ORTO_REGIONS[%s].is_stack = 0;" slot_var;
        Printf.sprintf
          "Region %s = ((Region){ .slot = %s, .expected_gen = ORTO_REGIONS[%s].gen });"
          r_var slot_var slot_var;
      ] in
      { stmts; value = r_var }

  | Check.T.TEIndex (arr_e, idx_e, elem_ty) ->
      let (ca, ci, a_var, i_var, arr_c, checks, slot_expr) =
        index_setup ctor_map arr_e idx_e (c_type elem_ty)
      in
      let result_var = fresh "_idx" in
      let stmts = ca.stmts @ ci.stmts @ [
        Printf.sprintf "%s %s = %s;" arr_c a_var ca.value;
        Printf.sprintf "int %s = %s;" i_var ci.value;
      ] @ checks @ [
        Printf.sprintf "%s %s = %s;" (c_type elem_ty) result_var slot_expr;
      ] in
      { stmts; value = result_var }

  | Check.T.TEAssignIdx (arr_e, idx_e, val_e, _) ->
      let (ca, ci, a_var, i_var, arr_c, checks, slot_expr) =
        index_setup ctor_map arr_e idx_e (c_type (ty_of_expr val_e))
      in
      let cv = emit_expr ctor_map val_e in
      let stmts = ca.stmts @ ci.stmts @ cv.stmts @ [
        Printf.sprintf "%s %s = %s;" arr_c a_var ca.value;
        Printf.sprintf "int %s = %s;" i_var ci.value;
      ] @ checks @ [
        Printf.sprintf "%s = %s;" slot_expr cv.value;
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

  | Check.T.TESlice (arr_e, lo_e, hi_e, result_ty) ->
      (* slice(a, lo, hi): produce a new handle into the same region.
         Bounds: 0 <= lo <= hi <= len. Gen check still happens — the
         slice is alive only while the source region is alive. The
         element type carries through, so slice(s: Array[byte], ...)
         returns Array[byte]; slice(xs: Array[T], ...) returns Array[T]. *)
      let ca  = emit_expr ctor_map arr_e in
      let clo = emit_expr ctor_map lo_e in
      let chi = emit_expr ctor_map hi_e in
      let a_var  = fresh "_a"  in
      let lo_var = fresh "_lo" in
      let hi_var = fresh "_hi" in
      let res_var = fresh "_sl" in
      let arr_c = c_type result_ty in
      let elem_c =
        match result_ty with
        | TyApp ("Array", [inner]) -> c_type inner
        | _ -> failwith "emit TESlice: result not Array[_]"
      in
      let stmts = ca.stmts @ clo.stmts @ chi.stmts @ [
        Printf.sprintf "%s %s = %s;" arr_c a_var ca.value;
        Printf.sprintf "int %s = %s;" lo_var clo.value;
        Printf.sprintf "int %s = %s;" hi_var chi.value;
        Printf.sprintf
          "if (ORTO_REGIONS[%s.slot].gen != %s.expected_gen) abort();"
          a_var a_var;
        Printf.sprintf
          "if (%s < 0 || %s < %s || %s > %s.len) abort();"
          lo_var hi_var lo_var hi_var a_var;
        Printf.sprintf
          "%s %s = ((%s){ .slot = %s.slot, .offset = %s.offset + %s * (int)sizeof(%s), .len = %s - %s, .expected_gen = %s.expected_gen });"
          arr_c res_var arr_c a_var a_var lo_var elem_c hi_var lo_var a_var;
      ] in
      { stmts; value = res_var }

  | Check.T.TEToInt sub ->
      let cs = emit_expr ctor_map sub in
      { stmts = cs.stmts;
        value = Printf.sprintf "((int)(%s))" cs.value }

  | Check.T.TEToByte sub ->
      let cs = emit_expr ctor_map sub in
      { stmts = cs.stmts;
        value = Printf.sprintf "((uint8_t)(%s))" cs.value }

  | Check.T.TECAlloc (et, n_e, _result_ty) ->
      let cn = emit_expr ctor_map n_e in
      let elem_c = c_type et in
      let value =
        Printf.sprintf "((%s*)malloc((size_t)(%s) * sizeof(%s)))"
          elem_c cn.value elem_c
      in
      { stmts = cn.stmts; value }

  | Check.T.TECFree p_e ->
      let cp = emit_expr ctor_map p_e in
      (* Wrap in comma expression so result is int (placeholder for unit). *)
      let value = Printf.sprintf "(free(%s), 0)" cp.value in
      { stmts = cp.stmts; value }

  | Check.T.TENullPtr t ->
      { stmts = []; value = Printf.sprintf "((%s)NULL)" (c_type t) }

  | Check.T.TEIsNull p_e ->
      let cp = emit_expr ctor_map p_e in
      { stmts = cp.stmts;
        value = Printf.sprintf "((%s) == NULL)" cp.value }

  | Check.T.TEArrayData (a_e, result_ty) ->
      (* array_data(a) — produce a raw *T pointing at the first element
         of the Array[T] in its region. Still gen-checks: passing
         dangling bytes to C would crash. *)
      let ca = emit_expr ctor_map a_e in
      let a_var = fresh "_a" in
      let arr_c = c_type (ty_of_expr a_e) in
      let elem_c = match result_ty with
        | TyPtr inner -> c_type inner
        | _ -> failwith "emit TEArrayData: result not *T"
      in
      let res_var = fresh "_data" in
      let stmts = ca.stmts @ [
        Printf.sprintf "%s %s = %s;" arr_c a_var ca.value;
        Printf.sprintf
          "if (ORTO_REGIONS[%s.slot].gen != %s.expected_gen) abort();"
          a_var a_var;
        Printf.sprintf
          "%s* %s = (%s*)(ORTO_REGIONS[%s.slot].buffer + %s.offset);"
          elem_c res_var elem_c a_var a_var;
      ] in
      { stmts; value = res_var }

  | Check.T.TEDeref (p_e, _) ->
      let cp = emit_expr ctor_map p_e in
      { stmts = cp.stmts;
        value = Printf.sprintf "(*%s)" cp.value }

  | Check.T.TEAssign (x, v_e, _) ->
      let cv = emit_expr ctor_map v_e in
      let stmts = cv.stmts @ [Printf.sprintf "%s = %s;" x cv.value] in
      { stmts; value = "0" }

  | Check.T.TEWhile (cond_e, body_e) ->
      (* Emit cond at the top of each iteration. C's while requires a
         pure expression in the head; if cond has side-effect stmts,
         we move them inside the loop with a break-on-false pattern. *)
      let cc = emit_expr ctor_map cond_e in
      let cb = emit_expr ctor_map body_e in
      let indent ss = List.map (fun s -> "    " ^ s) ss in
      let stmts =
        if cc.stmts = [] then
          [Printf.sprintf "while (%s) {" cc.value]
          @ indent cb.stmts
          @ [Printf.sprintf "    (void)(%s);" cb.value]
          @ ["}"]
        else
          ["while (1) {"]
          @ indent cc.stmts
          @ [Printf.sprintf "    if (!(%s)) break;" cc.value]
          @ indent cb.stmts
          @ [Printf.sprintf "    (void)(%s);" cb.value]
          @ ["}"]
      in
      { stmts; value = "0" }

  | Check.T.TEBreak    -> { stmts = ["break;"];    value = "0" }
  | Check.T.TEContinue -> { stmts = ["continue;"]; value = "0" }

  | Check.T.TEReturn (v_e, _) ->
      let cv = emit_expr ctor_map v_e in
      let stmts = cv.stmts @ [Printf.sprintf "return %s;" cv.value] in
      { stmts; value = "0" }

  | Check.T.TEDrop (sub, t) ->
      let cs = emit_expr ctor_map sub in
      let var = fresh "_drop_val" in
      let c_ty = c_type t in
      let stmts = cs.stmts @ [
        Printf.sprintf "%s %s = %s;" c_ty var cs.value;
        drop_call_stmt var t;
      ] in
      { stmts; value = "0" }

  | Check.T.TETryAt (a_e, i_e, result_ty) ->
      (* try_at(a, i): Some(a[i]) if gen+bounds OK, else None. *)
      let ca = emit_expr ctor_map a_e in
      let ci = emit_expr ctor_map i_e in
      let a_var = fresh "_a" in
      let i_var = fresh "_i" in
      let res_var = fresh "_try" in
      let arr_c = c_type (ty_of_expr a_e) in
      let opt_c = c_type result_ty in
      let elem_ty = match ty_of_expr a_e with
        | TyApp ("Array", [inner]) -> inner
        | _ -> failwith "emit TETryAt: scrutinee not Array[_]"
      in
      let elem_c = c_type elem_ty in
      let stmts = ca.stmts @ ci.stmts @ [
        Printf.sprintf "%s %s = %s;" arr_c a_var ca.value;
        Printf.sprintf "int %s = %s;" i_var ci.value;
        Printf.sprintf "%s %s;" opt_c res_var;
        Printf.sprintf
          "if (ORTO_REGIONS[%s.slot].gen == %s.expected_gen \
           && %s >= 0 && %s < %s.len) {"
          a_var a_var i_var i_var a_var;
        Printf.sprintf
          "    %s = ((%s){ .tag = 0, .as = { .Some = { .f0 = \
           ((%s*)(ORTO_REGIONS[%s.slot].buffer + %s.offset))[%s] } } });"
          res_var opt_c elem_c a_var a_var i_var;
        "} else {";
        Printf.sprintf "    %s = ((%s){ .tag = 1 });" res_var opt_c;
        "}";
      ] in
      { stmts; value = res_var }

(* Shared setup for a[i] and a[i] := v. Returns the array/index codes,
   fresh names, the array's C type, the abort-checks, and the C
   expression for the slot at index. *)
and index_setup ctor_map arr_e idx_e elem_c =
  let ca = emit_expr ctor_map arr_e in
  let ci = emit_expr ctor_map idx_e in
  let a_var = fresh "_a" in
  let i_var = fresh "_i" in
  let arr_c = c_type (ty_of_expr arr_e) in
  match ty_of_expr arr_e with
  | TyApp ("Array", _) ->
      let checks = [
        Printf.sprintf
          "if (ORTO_REGIONS[%s.slot].gen != %s.expected_gen) abort();"
          a_var a_var;
        Printf.sprintf "if (%s < 0 || %s >= %s.len) abort();"
          i_var i_var a_var;
      ] in
      let slot =
        Printf.sprintf
          "((%s*)(ORTO_REGIONS[%s.slot].buffer + %s.offset))[%s]"
          elem_c a_var a_var i_var
      in
      (ca, ci, a_var, i_var, arr_c, checks, slot)
  | TyPtr _ ->
      (* Raw pointer indexing — no gen, no bounds. The slot expression
         doesn't use elem_c (the pointer type already carries it), but
         we keep the parameter for symmetry with the Array branch. *)
      let _ = elem_c in
      let slot = Printf.sprintf "%s[%s]" a_var i_var in
      (ca, ci, a_var, i_var, arr_c, [], slot)
  | t ->
      failwith (Printf.sprintf
        "emit: indexing on non-indexable type %s" (Ast.show_ty t))

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
    if f.param_drops = [] then
      cb.stmts @ [Printf.sprintf "return %s;" cb.value]
    else
      let ret_var = "_ret" in
      let param_drop_calls =
        List.filter_map (fun pname ->
          match List.assoc_opt pname f.params with
          | Some pt -> Some (drop_call_stmt pname pt)
          | None -> None) f.param_drops
      in
      cb.stmts
      @ [Printf.sprintf "%s %s = %s;"
           (c_type f.return_ty) ret_var cb.value]
      @ param_drop_calls
      @ [Printf.sprintf "return %s;" ret_var]
  in
  let indented = List.map (fun s -> "    " ^ s) body_lines in
  Printf.sprintf "%s %s(%s) {\n%s\n}"
    (c_type f.return_ty) f.name params_s
    (String.concat "\n" indented)

(* Render the static byte pool as a C array literal. Every "..." literal
   from the program ends up in this buffer at its assigned offset. The
   buffer is never freed; its gen stays at 1 forever. *)
let emit_static_bytes_array () : string =
  let total = if !string_pool_size = 0 then 1 else !string_pool_size in
  let buf = Bytes.make total '\x00' in
  List.iter (fun (s, off) ->
    String.iteri (fun i c -> Bytes.set buf (off + i) c) s)
    !string_pool_order;
  let chars =
    List.init total (fun i ->
      Printf.sprintf "0x%02x" (Char.code (Bytes.get buf i)))
  in
  Printf.sprintf
    "static const uint8_t ORTO_STATIC_BYTES[%d] = { %s };\n\
     #define ORTO_STATIC_BYTES_LEN %d"
    total (String.concat ", " chars) !string_pool_size

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
  let static_bytes = emit_static_bytes_array () in
  let header = Printf.sprintf
    "/* generated by orto */\n\
     #include <stdlib.h>\n\
     #include <stddef.h>\n\
     #include <stdint.h>\n\
     #include <string.h>\n\
     \n\
     /* Region runtime: a global slab of region slots. Each slot is\n\
      * reused after its region is dropped (gen bumps so old handles\n\
      * see a mismatch and either abort or take the dangling branch).\n\
      * No allocation per region beyond the user-requested buffer.\n\
      * Slot 0 is reserved for the static string-literal pool. */\n\
     #define ORTO_REGION_SLOTS 4096\n\
     struct Region_slot {\n\
     \    int gen;\n\
     \    char* buffer;\n\
     \    size_t buffer_size;\n\
     \    size_t used;\n\
     \    int next_free;   /* -1 if in use, else next free slot id */\n\
     \    int is_stack;    /* 1 if buffer is stack memory (do not free) */\n\
     };\n\
     typedef struct { int slot; int expected_gen; } Region;\n\
     static struct Region_slot ORTO_REGIONS[ORTO_REGION_SLOTS];\n\
     static int ORTO_REGION_FREE_HEAD = -1;\n\
     \n\
     static void drop_Region(Region r) {\n\
     \    if (ORTO_REGIONS[r.slot].gen != r.expected_gen) return;\n\
     \    if (!ORTO_REGIONS[r.slot].is_stack)\n\
     \        free(ORTO_REGIONS[r.slot].buffer);\n\
     \    ORTO_REGIONS[r.slot].buffer = NULL;\n\
     \    ORTO_REGIONS[r.slot].buffer_size = 0;\n\
     \    ORTO_REGIONS[r.slot].used = 0;\n\
     \    ORTO_REGIONS[r.slot].gen++;\n\
     \    ORTO_REGIONS[r.slot].next_free = ORTO_REGION_FREE_HEAD;\n\
     \    ORTO_REGION_FREE_HEAD = r.slot;\n\
     }\n\
     \n\
     %s\n\
     \n\
     static void orto_init_regions(void) __attribute__((constructor));\n\
     static void orto_init_regions(void) {\n\
     \    for (int i = 0; i < ORTO_REGION_SLOTS; i++) {\n\
     \        ORTO_REGIONS[i].gen = 1;\n\
     \        ORTO_REGIONS[i].next_free = i + 1;\n\
     \    }\n\
     \    ORTO_REGIONS[ORTO_REGION_SLOTS - 1].next_free = -1;\n\
     \    /* slot 0 = static string pool, never freed */\n\
     \    ORTO_REGIONS[0].buffer = (char*)ORTO_STATIC_BYTES;\n\
     \    ORTO_REGIONS[0].buffer_size = sizeof(ORTO_STATIC_BYTES);\n\
     \    ORTO_REGIONS[0].used = ORTO_STATIC_BYTES_LEN;\n\
     \    ORTO_REGIONS[0].is_stack = 1;\n\
     \    ORTO_REGIONS[0].next_free = -1;\n\
     \    ORTO_REGION_FREE_HEAD = 1;\n\
     }"
    static_bytes
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

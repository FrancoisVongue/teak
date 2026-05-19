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

(* Task[T] / Stream[T] instantiations. The C struct is opaque for now
   (placeholder field) — async-extern lowering never reads it. Spawn
   (later phase) gives it concrete contents. *)
let task_wrappers_seen : (string, unit) Hashtbl.t = Hashtbl.create 4
let task_wrappers_order : string list ref = ref []
let stream_wrappers_seen : (string, unit) Hashtbl.t = Hashtbl.create 4
let stream_wrappers_order : string list ref = ref []

(* Tuple shapes: one C typedef per distinct mono tuple type. Keyed by
   mangled name; carries the component type list so we can render
   field declarations. *)
let tuple_types_seen : (string, unit) Hashtbl.t = Hashtbl.create 4
let tuple_types_order : (string * ty list) list ref = ref []

let register_tuple (mangled : string) (ts : ty list) =
  if not (Hashtbl.mem tuple_types_seen mangled) then begin
    Hashtbl.add tuple_types_seen mangled ();
    tuple_types_order := (mangled, ts) :: !tuple_types_order
  end

let register_task_wrapper (inner_mangled : string) =
  if not (Hashtbl.mem task_wrappers_seen inner_mangled) then begin
    Hashtbl.add task_wrappers_seen inner_mangled ();
    task_wrappers_order := inner_mangled :: !task_wrappers_order
  end

let register_stream_wrapper (inner_mangled : string) =
  if not (Hashtbl.mem stream_wrappers_seen inner_mangled) then begin
    Hashtbl.add stream_wrappers_seen inner_mangled ();
    stream_wrappers_order := inner_mangled :: !stream_wrappers_order
  end

(* String literal pool. Every "..." in the program is deduplicated and
   assigned a byte-offset into one shared static buffer. The buffer
   sits at region slot 0 (reserved at startup, never freed). Each
   literal emits to an Array[byte] handle with .slot=0, the offset,
   length, and the static gen=1. *)
let string_pool : (string, int) Hashtbl.t = Hashtbl.create 16
let string_pool_order : (string * int) list ref = ref []
let string_pool_size = ref 0

(* Names of `extern async fn` declarations. Used by the async lowering
   to recognise calls that should compile to an SQE prep + submit
   pattern, with the current frame as user_data. *)
let async_externs : (string, unit) Hashtbl.t = Hashtbl.create 8

let register_async_extern (name : string) =
  Hashtbl.replace async_externs name ()

let is_async_extern (name : string) : bool =
  Hashtbl.mem async_externs name

(* Subset of async externs that are stream-shaped: the C-side glue
   preps a multishot SQE and many CQEs land on the same user_data
   pointer. Source-visible signature returns Stream[T], driven by
   `for x in call(...) { ... }`. *)
let stream_externs : (string, unit) Hashtbl.t = Hashtbl.create 4

let register_stream_extern (name : string) =
  Hashtbl.replace stream_externs name ()

let is_stream_extern (name : string) : bool =
  Hashtbl.mem stream_externs name

(* Map of async-function names to their parameter list and return
   type. Populated in collect_program for every monomorphised function
   whose body is async (i.e. lowered into a Frame_<name> + <name>_step).
   spawn / await sites read it for arg initialisation and for the
   T-typed conversion across the long long header slot. *)
let async_func_params : (string, (string * ty) list) Hashtbl.t =
  Hashtbl.create 8
let async_func_returns : (string, ty) Hashtbl.t = Hashtbl.create 8

let register_async_func (name : string) (params : (string * ty) list) (ret_ty : ty) =
  Hashtbl.replace async_func_params name params;
  Hashtbl.replace async_func_returns name ret_ty

let is_async_func (name : string) : bool =
  Hashtbl.mem async_func_params name

let async_func_return_ty (name : string) : ty option =
  Hashtbl.find_opt async_func_returns name

(* Is the type scalar-shaped — fits in long long, can be assigned via
   implicit C conversion? Aggregate types (Region, Task[T], records,
   Arrays) need memcpy to / from the long long header slot. *)
let scalar_like (t : ty) : bool =
  match t with
  | TyInt | TyBool -> true
  | TyApp ("byte", []) -> true
  | TyApp ("float", []) -> true
  | TyPtr _ -> true
  | _ -> false

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
  | TyApp ("u16", []) | TyApp ("u32", []) | TyApp ("u64", []) -> ()
      (* unsigned primitives; map to uint{16,32,64}_t in C. *)
  | TyApp ("__cll", []) -> ()
      (* internal pseudo-type for `long long` frame fields *)
  | TyApp ("byte", _) ->
      failwith "emit collect_ty: byte takes no type arguments"
  | TyApp ("float", []) -> ()
      (* float is a primitive; maps directly to double in C. *)
  | TyApp ("float", _) ->
      failwith "emit collect_ty: float takes no type arguments"
  | TyApp ("Task", [inner]) ->
      (* Stage 3 phase 4: collect the inner element and register the
         Task wrapper so c_type has a real C name to refer to. The
         struct stays opaque for now — concrete fields land with
         `spawn` (slot index + gen). Async-extern calls never read
         this struct; they only need its name to live in signatures. *)
      collect_ty inner;
      register_task_wrapper (Mono.mangle_ty inner)
  | TyApp ("Task", _) ->
      failwith "emit collect_ty: Task with wrong arity"
  | TyApp ("Stream", [inner]) ->
      collect_ty inner;
      register_stream_wrapper (Mono.mangle_ty inner)
  | TyApp ("Stream", _) ->
      failwith "emit collect_ty: Stream with wrong arity"
  | TyApp (_, []) -> ()
  | TyApp (n, _) ->
      failwith (Printf.sprintf "emit collect_ty: %S still has args" n)
  | TyTuple ts ->
      List.iter collect_ty ts;
      register_tuple (Mono.mangle_ty t) ts
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
  | Check.T.TEInt _ | Check.T.TEFloat _ | Check.T.TEBool _ -> ()
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
      List.iter (fun (_, g, body) ->
        Option.iter collect_expr g;
        collect_expr body) arms;
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
  | Check.T.TEToU16 e  -> collect_expr e
  | Check.T.TEToU32 e  -> collect_expr e
  | Check.T.TEToU64 e  -> collect_expr e
  | Check.T.TEToFloat e -> collect_expr e
  | Check.T.TEToIntFromFloat e -> collect_expr e
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
  | Check.T.TEAwait (e, t, p) -> collect_expr e; collect_ty t; collect_ty p
  | Check.T.TESpawn (e, t) -> collect_expr e; collect_ty t
  | Check.T.TEForStream (_, et, s, b) ->
      collect_ty et; collect_expr s; collect_expr b
  | Check.T.TETuple (es, t) ->
      List.iter collect_expr es; collect_ty t
  | Check.T.TETupleIdx (e, _, t) ->
      collect_expr e; collect_ty t
  | Check.T.TELetTuple (_, vt, v, b, bt, _) ->
      collect_ty vt; collect_expr v; collect_expr b; collect_ty bt
  | Check.T.TEAwaitAll (bs, t, ptys) ->
      List.iter collect_expr bs; collect_ty t;
      List.iter collect_ty ptys

let collect_program (prog : Check.T.program) : unit =
  Hashtbl.clear fn_types_seen;
  fn_types_order := [];
  Hashtbl.clear array_types_seen;
  array_types_order := [];
  Hashtbl.clear task_wrappers_seen;
  task_wrappers_order := [];
  Hashtbl.clear stream_wrappers_seen;
  stream_wrappers_order := [];
  Hashtbl.clear tuple_types_seen;
  tuple_types_order := [];
  Hashtbl.clear string_pool;
  string_pool_order := [];
  string_pool_size := 0;
  Hashtbl.clear async_externs;
  Hashtbl.clear stream_externs;
  Hashtbl.clear async_func_params;
  List.iter (fun td ->
    List.iter (fun v ->
      List.iter collect_ty v.arg_tys) td.variants) prog.types;
  List.iter (fun (rd : record_decl) ->
    List.iter (fun (_, t) -> collect_ty t) rd.rec_fields) prog.records;
  List.iter (fun (e : Check.T.extern) ->
    List.iter (fun (_, t) -> collect_ty t) e.params;
    collect_ty e.return_ty;
    if e.is_async then register_async_extern e.name;
    if e.is_stream then register_stream_extern e.name) prog.externs;
  List.iter (fun (f : Check.T.func) ->
    List.iter (fun (_, t) -> collect_ty t) f.params;
    collect_ty f.return_ty;
    collect_expr f.body;
    if f.is_async then register_async_func f.name f.params f.return_ty) prog.funcs

(* ---------- rendering C types ---------- *)

let rec c_type (t : ty) : string =
  match t with
  | TyInt -> "int"
  | TyBool -> "int"
  | TyApp ("byte", []) -> "uint8_t"
  | TyApp ("u16", [])  -> "uint16_t"
  | TyApp ("u32", [])  -> "uint32_t"
  | TyApp ("u64", [])  -> "uint64_t"
  | TyApp ("float", []) -> "double"
  (* Compiler-internal pseudo-type. Never appears in user surface;
     used by emit to mark frame fields that must hold a 64-bit gen
     counter so per-slot wrap can't false-match an old handle. *)
  | TyApp ("__cll", []) -> "long long"
  | TyApp ("Array", [inner]) -> mangle_array_name inner
  | TyApp ("Region", []) -> "Region"
  | TyApp ("Task", [inner]) ->
      (* Stage 3 phase 2 placeholder: the concrete C struct for a
         Task[T] is generated in phase 4 alongside the state machine.
         For now we mangle a name so type machinery can mention it. *)
      "Task_" ^ Mono.mangle_ty inner
  | TyApp ("Stream", [inner]) ->
      "Stream_" ^ Mono.mangle_ty inner
  | TyApp (n, []) -> n
  | TyFun _ -> Mono.mangle_ty t
  | TyPtr inner -> c_type inner ^ "*"
  | TyTuple _ -> Mono.mangle_ty t
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
      "typedef struct { int slot; int offset; int len; long long expected_gen; } %s;"
      mangled)
    !array_types_order

(* Task[T] / Stream[T] opaque wrappers — one C struct per instantiation
   so c_type has a name to refer to. Phase 4 (await) doesn't use the
   contents; later phases (spawn / for-in stream) flesh them out. *)
let emit_task_forwards () : string list =
  let task_fwd =
    List.rev_map (fun m ->
      Printf.sprintf
        "typedef struct { int slot; long long gen; } Task_%s;" m)
      !task_wrappers_order
  in
  let stream_fwd =
    List.rev_map (fun m ->
      Printf.sprintf
        "typedef struct { int slot; long long gen; } Stream_%s;" m)
      !stream_wrappers_order
  in
  task_fwd @ stream_fwd

(* One typedef per distinct tuple shape. Tuples are anonymous structural
   products — `(int, bool, byte)` mangles to `Tuple_int_bool_byte` and
   expands to a C struct with fields f0, f1, f2.  Order matters for C
   only when one tuple type appears as a field type of another; we emit
   in reverse-insertion order (deepest child first), same trick as the
   array typedefs. *)
let emit_tuple_forwards () : string list =
  List.rev_map (fun (mangled, ts) ->
    let fields =
      String.concat " "
        (List.mapi (fun i ty ->
          Printf.sprintf "%s f%d;" (c_type ty) i) ts)
    in
    Printf.sprintf "typedef struct { %s } %s;" fields mangled)
    !tuple_types_order

(* drop_<TupleX> for every tuple shape that contains a linear component.
   Walks the tuple's components and drops each linear one in turn. *)
let emit_tuple_drop_forwards () : string list =
  List.rev_map (fun (mangled, ts) ->
    if List.exists Check.is_linear_ty ts then
      Some (Printf.sprintf "static void drop_%s(%s t);" mangled mangled)
    else None)
    !tuple_types_order
  |> List.filter_map (fun x -> x)

let emit_tuple_drop_defs () : string list =
  List.rev_map (fun (mangled, ts) ->
    if not (List.exists Check.is_linear_ty ts) then None
    else
      let drops =
        List.mapi (fun i ty ->
          if Check.is_linear_ty ty then
            Some (Printf.sprintf "    %s"
                    (let _ = ty in
                     let fn_call = match ty with
                       | TyApp ("Array", [inner]) ->
                           Printf.sprintf "drop_%s(t.f%d);" (mangle_array_name inner) i
                       | TyApp ("Task", [inner]) ->
                           Printf.sprintf "drop_Task_%s(t.f%d);" (Mono.mangle_ty inner) i
                       | TyApp ("Stream", [inner]) ->
                           Printf.sprintf "drop_Stream_%s(t.f%d);" (Mono.mangle_ty inner) i
                       | TyTuple _ ->
                           Printf.sprintf "drop_%s(t.f%d);" (Mono.mangle_ty ty) i
                       | TyApp (n, _) ->
                           Printf.sprintf "%s(t.f%d);" (Check.drop_fn_name_for n) i
                       | _ -> ""
                     in fn_call))
          else None) ts
        |> List.filter_map (fun x -> x)
      in
      Some (Printf.sprintf
        "static void drop_%s(%s t) {\n%s\n}"
        mangled mangled (String.concat "\n" drops)))
    !tuple_types_order
  |> List.filter_map (fun x -> x)

(* drop_Task_<T> for every Task[T] instantiation. Dropping a held
   Task without an `await` turns the joinable task into a detached
   one — the worker keeps running and self-frees its slot. If the
   task already finished (DONE_NO_WAITER), free the slot right now.
   If its generation no longer matches, it's already gone. *)
let emit_task_drop_defs () : string list =
  List.rev_map (fun m ->
    Printf.sprintf
      "static void drop_Task_%s(Task_%s t) {\n\
       \    if (ORTO_SLOTS[t.slot].gen != t.gen) return;\n\
       \    if (ORTO_SLOTS[t.slot].status == ORTO_SLOT_DONE_NO_WAITER) {\n\
       \        orto_slot_free(t.slot);\n\
       \    } else {\n\
       \        ORTO_SLOTS[t.slot].status = ORTO_SLOT_DETACHED;\n\
       \    }\n\
       }"
      m m)
    !task_wrappers_order

let emit_task_drop_forwards () : string list =
  List.rev_map (fun m ->
    Printf.sprintf "static void drop_Task_%s(Task_%s t);" m m)
    !task_wrappers_order

(* drop_Stream_<T> for every Stream[T] instantiation. v1 policy:
   marking the slot DETACHED — the multishot SQE keeps firing until
   the source closes itself; the worker self-frees. A cleaner future
   step is an explicit IORING_OP_ASYNC_CANCEL to tear down the SQE
   before any further CQEs land. *)
let emit_stream_drop_defs () : string list =
  List.rev_map (fun m ->
    Printf.sprintf
      "static void drop_Stream_%s(Stream_%s s) {\n\
       \    if (ORTO_SLOTS[s.slot].gen != s.gen) return;\n\
       \    /* TODO: prep ASYNC_CANCEL SQE to stop further CQEs. */\n\
       \    ORTO_SLOTS[s.slot].status = ORTO_SLOT_DETACHED;\n\
       }"
      m m)
    !stream_wrappers_order

let emit_stream_drop_forwards () : string list =
  List.rev_map (fun m ->
    Printf.sprintf "static void drop_Stream_%s(Stream_%s s);" m m)
    !stream_wrappers_order

(* Emit a call to the right drop function for a linear type. After mono,
   the type name carries its module mangling (`net__Socket`); the helper
   in check.ml derives the matching drop fn name. For Region the runtime
   supplies `drop_Region` directly. Array[Linear T] gets a generated
   drop_Array_<T> per instantiation (phase 3 induced linearity). *)
let drop_call_stmt (var_name : string) (t : ty) : string =
  match t with
  | TyApp ("Array", [inner]) ->
      let fn = "drop_" ^ mangle_array_name inner in
      Printf.sprintf "%s(%s);" fn var_name
  | TyApp ("Task", [inner]) ->
      let fn = "drop_Task_" ^ Mono.mangle_ty inner in
      Printf.sprintf "%s(%s);" fn var_name
  | TyApp ("Stream", [inner]) ->
      let fn = "drop_Stream_" ^ Mono.mangle_ty inner in
      Printf.sprintf "%s(%s);" fn var_name
  | TyApp (n, _) ->
      let fn = Check.drop_fn_name_for n in
      Printf.sprintf "%s(%s);" fn var_name
  | TyTuple _ ->
      let fn = "drop_" ^ Mono.mangle_ty t in
      Printf.sprintf "%s(%s);" fn var_name
  | _ ->
      failwith
        (Printf.sprintf "emit: drop on non-TyApp type %s" (Ast.show_ty t))

(* Forward declarations for every drop_Array_<T> we'll emit, so they
   can be referenced before their definition (e.g. nested
   Array[Array[Linear]] drops the inner array). *)
let emit_array_drop_forwards () : string list =
  List.rev_map (fun (mangled, inner) ->
    if Check.is_linear_ty inner then
      Some (Printf.sprintf "static void drop_%s(%s a);" mangled mangled)
    else None)
    !array_types_order
  |> List.filter_map (fun x -> x)

(* Cascade drop for Array[T] when T is linear: walks the live slots
   of the element backing buffer (gen-checked) and drops each. The
   array handle itself doesn't free memory — the surrounding Region
   does that. *)
let emit_array_drop_defs () : string list =
  List.rev_map (fun (mangled, inner) ->
    if not (Check.is_linear_ty inner) then None
    else
      let elem_c = c_type inner in
      let drop_elem = drop_call_stmt "data[i]" inner in
      Some (Printf.sprintf
        "static void drop_%s(%s a) {\n\
         \    if (ORTO_REGIONS[a.slot].gen != a.expected_gen) return;\n\
         \    %s* data = (%s*)(ORTO_REGIONS[a.slot].buffer + a.offset);\n\
         \    for (int i = 0; i < a.len; i++) {\n\
         \        %s\n\
         \    }\n\
         }"
        mangled mangled elem_c elem_c drop_elem))
    !array_types_order
  |> List.filter_map (fun x -> x)

(* ---------- operator C-strings ---------- *)

let c_binop = function
  | OpAdd -> "+"  | OpSub -> "-"
  | OpMul -> "*"  | OpDiv -> "/"  | OpMod -> "%"
  | OpEq  -> "==" | OpNeq -> "!="
  | OpLt  -> "<"  | OpGt  -> ">"
  | OpLe  -> "<=" | OpGe  -> ">="
  | OpAnd -> "&&" | OpOr  -> "||"
  | OpBOr -> "|"  | OpBAnd -> "&" | OpBXor -> "^"
  | OpShl -> "<<" | OpShr -> ">>"

let c_unop = function
  | OpNeg  -> "-"
  | OpNot  -> "!"
  | OpBNot -> "~"

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
    | TEInt _ | TEFloat _ | TEBool _ | TEStringLit _ | TEFnRef _ -> e
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
        let rn_arm (_, guard, body) (new_p, env') =
          let g' = Option.map (rn env') guard in
          (new_p, g', rn env' body)
        in
        let arms' =
          List.map (fun (p, guard, body) ->
            match p with
            | POr _ | PInt _ | PBool _ | PStr _ | PBind "_" ->
                rn_arm (p, guard, body) (p, env)
            | PBind x ->
                let x' = fresh x in
                let env' = (x, x') :: env in
                rn_arm (p, guard, body) (PBind x', env')
            | PCtor (c, names) ->
                let pairs =
                  List.map (fun n ->
                    if n = "_" then (n, n) else (n, fresh n)) names
                in
                let new_names = List.map snd pairs in
                let env' = pairs @ env in
                rn_arm (p, guard, body) (PCtor (c, new_names), env')
            | PTuple ps ->
                let pairs = ref [] in
                let rec rn_p p =
                  match p with
                  | PBind "_" -> PBind "_"
                  | PBind x ->
                      let x' = fresh x in
                      pairs := (x, x') :: !pairs;
                      PBind x'
                  | PTuple sub -> PTuple (List.map rn_p sub)
                  | _ -> p
                in
                let p' = PTuple (List.map rn_p ps) in
                let env' = !pairs @ env in
                rn_arm (p, guard, body) (p', env')) arms
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
    | TEToU16 e  -> TEToU16 (rn env e)
    | TEToU32 e  -> TEToU32 (rn env e)
    | TEToU64 e  -> TEToU64 (rn env e)
    | TEToFloat e -> TEToFloat (rn env e)
    | TEToIntFromFloat e -> TEToIntFromFloat (rn env e)
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
    | TEAwait (e, t, p) -> TEAwait (rn env e, t, p)
    | TESpawn (e, t) -> TESpawn (rn env e, t)
    | TEForStream (x, et, s, b) ->
        let s' = rn env s in
        if x = "_" then
          TEForStream ("_", et, s', rn env b)
        else
          let x' = fresh x in
          let env' = (x, x') :: env in
          TEForStream (x', et, s', rn env' b)
    | TETuple (es, t) -> TETuple (List.map (rn env) es, t)
    | TETupleIdx (e, i, t) -> TETupleIdx (rn env e, i, t)
    | TELetTuple (names, vt, v, b, bt, ads) ->
        let v' = rn env v in
        let renames =
          List.map (fun n -> if n = "_" then n else fresh n) names
        in
        let env' =
          List.fold_left2 (fun acc orig fresh_name ->
            if orig = "_" then acc else (orig, fresh_name) :: acc)
            env names renames
        in
        TELetTuple (renames, vt, v', rn env' b, bt, ads)
    | TEAwaitAll (bs, t, ptys) ->
        TEAwaitAll (List.map (rn env) bs, t, ptys)
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
    | TyTuple _ -> acc (* tuples are structural; topo sort treats them
                          as transparent — they will be typedef'd later. *)
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
  | Check.T.TEFloat _ -> TyApp ("float", [])
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
  | Check.T.TEToU16 _  -> TyApp ("u16", [])
  | Check.T.TEToU32 _  -> TyApp ("u32", [])
  | Check.T.TEToU64 _  -> TyApp ("u64", [])
  | Check.T.TEToFloat _ -> TyApp ("float", [])
  | Check.T.TEToIntFromFloat _ -> TyInt
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
  | Check.T.TEAwait (_, t, _) -> t
  | Check.T.TESpawn (_, t) -> t
  | Check.T.TEForStream _ -> TyInt
  | Check.T.TETuple (_, t) -> t
  | Check.T.TETupleIdx (_, _, t) -> t
  | Check.T.TELetTuple (_, _, _, _, t, _) -> t
  | Check.T.TEAwaitAll (_, t, _) -> t

(* Release a Region's buffer (if it's heap-allocated), bump the
   generation, and push the slot back onto the free list. Stack
   regions skip the free — their storage is reclaimed when the
   surrounding C function returns. Used by let-scope auto_drop and
   function-end param drops. *)
let rec is_catchall_pat_emit = function
  | PBind _ -> true
  | PTuple ps -> List.for_all is_catchall_pat_emit ps
  | _ -> false

let rec emit_expr
  (ctor_map : (string, type_decl * variant * int) Hashtbl.t)
  (e : Check.T.expr) : c_code =
  match e with
  | Check.T.TEInt n      -> { stmts = []; value = string_of_int n }
  | Check.T.TEFloat f    ->
      (* Use enough digits to round-trip a double exactly. Force a
         decimal point so `1.0` doesn't emit as `1` (which C parses
         as int and then `1 / 0` truncates instead of producing NaN). *)
      let s = Printf.sprintf "%.17g" f in
      let needs_dot =
        not (String.contains s '.' || String.contains s 'e'
             || String.contains s 'E' || String.contains s 'n')
      in
      let s = if needs_dot then s ^ ".0" else s in
      { stmts = []; value = s }
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
      (* Bindings whose name starts with `fr->` come from
         async_rewrite_to_frame — the variable lives in the
         function's frame, so the let stores into the frame field
         rather than declaring a new C local. *)
      let is_frame_binder =
        String.length x > 4 && String.sub x 0 4 = "fr->"
      in
      let decl =
        if x = "_" then
          Printf.sprintf "(void)(%s);" cv.value
        else if is_frame_binder then
          Printf.sprintf "%s = %s;" x cv.value
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
        | TyApp ("u16", []) | TyApp ("u32", []) | TyApp ("u64", []) -> false
        | TyApp ("Array", _) -> false
        | TyTuple _ -> false
        | TyApp _ -> true
        | _ -> true
      in
      if is_adt_scrut then begin
        let emit_arm (pat, _guard, body) =
          (* Guards on ADT match are rejected in check.ml — no guard
             handling needed here. *)
          let bindings = match pat with
            | POr _ -> []
            | PBind "_" -> []
            | PBind x ->
                [Printf.sprintf "    %s %s = %s;" (c_type scrut_ty) x scrut_var]
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
          | PBind _ ->
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
        let has_catchall = List.exists (fun (p, _, _) ->
          match p with PBind _ -> true | _ -> false) arms in
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
                let off = register_string s in
                let len = String.length s in
                Printf.sprintf
                  "(%s).len == %d && memcmp(ORTO_REGIONS[(%s).slot].buffer + (%s).offset, ORTO_STATIC_BYTES + %d, %d) == 0"
                  scrut_var len scrut_var scrut_var off len
            | POr ps -> String.concat " || " (List.map single ps)
            | PBind _ -> "1"
            (* Tuple pattern: v1 only supports bind/wildcard sub-pats,
               so the whole pattern is always a match. The binding
               declarations land in bind_decl. *)
            | PTuple _ -> "1"
            | PCtor _ -> failwith "emit: ctor pattern in non-ADT match"
          in
          single pat
        in
        (* Walk a PTuple and collect (binder, c_type, accessor)
           triples for every named subbind. accessor is a C expression
           rooted in scrut_var. *)
        let rec tuple_bindings p access ty =
          (* mono has already concretized; ty has no metas. *)
          match p, ty with
          | PBind "_", _ -> []
          | PBind x, t -> [(x, c_type t, access)]
          | PTuple sub, TyTuple ts ->
              List.concat (List.mapi (fun i (sp, st) ->
                let acc = Printf.sprintf "((%s).f%d)" access i in
                tuple_bindings sp acc st)
                (List.combine sub ts))
          | _, _ ->
              failwith "emit: unsupported sub-pattern in tuple match"
        in
        (* Each arm sits in its own `{ ... }` block inside a
           `do { ... } while (0)`. A binding (PBind) becomes a local
           declaration ABOVE the guard test, so the guard can refer
           to the bound name. `break` exits the whole match. *)
        let emit_arm (pat, guard, body) =
          let cb = emit_expr ctor_map body in
          let bind_decl = match pat with
            | PBind x when x <> "_" ->
                [Printf.sprintf "    %s %s = %s;"
                   (c_type scrut_ty) x scrut_var]
            | PTuple _ ->
                List.map (fun (x, c_ty, access) ->
                  Printf.sprintf "    %s %s = %s;" c_ty x access)
                  (tuple_bindings pat scrut_var scrut_ty)
            | _ -> []
          in
          let test =
            let p = pat_test pat in
            match guard with
            | None -> p
            | Some g_e ->
                let cg = emit_expr ctor_map g_e in
                if cg.stmts <> [] then
                  failwith "emit: match guard with side-effect stmts not supported";
                Printf.sprintf "(%s) && (%s)" p cg.value
          in
          let body_lines =
            (List.map (fun s -> "        " ^ s) cb.stmts)
            @ [Printf.sprintf "        %s = %s;" result_var cb.value]
            @ ["        break;"]
          in
          ["{"]
          @ bind_decl
          @ [Printf.sprintf "    if (%s) {" test]
          @ body_lines
          @ ["    }"]
          @ ["}"]
        in
        let arm_blocks = List.concat_map emit_arm arms in
        let has_unguarded_catchall = List.exists (fun (p, g, _) ->
          is_catchall_pat_emit p && g = None) arms in
        let safety =
          if has_unguarded_catchall then []
          else ["abort();   /* exhaustive-by-construction safety net */"]
        in
        let stmts =
          cs.stmts
          @ [scrut_decl; result_decl]
          @ ["do {"]
          @ (List.map (fun s -> "    " ^ s) arm_blocks)
          @ (List.map (fun s -> "    " ^ s) safety)
          @ ["} while (0);"]
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

  | Check.T.TEToU16 sub ->
      let cs = emit_expr ctor_map sub in
      { stmts = cs.stmts;
        value = Printf.sprintf "((uint16_t)(%s))" cs.value }

  | Check.T.TEToU32 sub ->
      let cs = emit_expr ctor_map sub in
      { stmts = cs.stmts;
        value = Printf.sprintf "((uint32_t)(%s))" cs.value }

  | Check.T.TEToU64 sub ->
      let cs = emit_expr ctor_map sub in
      { stmts = cs.stmts;
        value = Printf.sprintf "((uint64_t)(%s))" cs.value }

  | Check.T.TEToFloat sub ->
      let cs = emit_expr ctor_map sub in
      { stmts = cs.stmts;
        value = Printf.sprintf "((double)(%s))" cs.value }

  | Check.T.TEToIntFromFloat sub ->
      let cs = emit_expr ctor_map sub in
      (* C cast double->int truncates toward zero. *)
      { stmts = cs.stmts;
        value = Printf.sprintf "((int)(%s))" cs.value }

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

  (* Stage 3 phase 2 lands typing only — the state-machine lowering
     and dispatcher runtime arrive in phases 4–5. Any program that
     reaches emit with await/spawn/yield is a compiler-state error
     unless we explicitly fail before typecheck succeeds. *)
  | Check.T.TEAwait _ ->
      failwith "emit TEAwait: Stage 3 phase 4 (state machine) not yet \
                implemented — function bodies using `await` cannot be \
                lowered yet"
  | Check.T.TESpawn (inner, spawn_ty) ->
      (* Spawn from a non-async caller. The work is kicked off the
         same way an async caller would do it; the difference is that
         the surrounding function has no dispatcher loop, so a
         spawned task that suspends will stay pending until the
         program's main eventually drains the ring. Callers who
         spawn from outside any async path are responsible for
         making sure a dispatcher actually runs. *)
      (match inner with
       | Check.T.TECall (Check.T.TEFnRef (worker, _, _), args, _)
         when is_async_func worker ->
           let arg_codes = List.map (emit_expr ctor_map) args in
           let stmts_pre =
             List.concat_map (fun c -> c.stmts) arg_codes
           in
           let arg_values = List.map (fun c -> c.value) arg_codes in
           let params =
             try Hashtbl.find async_func_params worker
             with Not_found ->
               failwith (Printf.sprintf
                 "emit: spawn target %S is not a known async function" worker)
           in
           if List.length params <> List.length arg_values then
             failwith (Printf.sprintf
               "emit: sync spawn %S: arg/param count mismatch" worker);
           let slot_v = fresh "_sp_slot" in
           let fr_v   = fresh "_sp_fr" in
           let rc_v   = fresh "_sp_rc" in
           let gen_v  = fresh "_sp_gen" in
           let param_inits =
             List.map2 (fun (p, _) v ->
               Printf.sprintf "%s->%s = %s;" fr_v p v) params arg_values
           in
           let stmts = stmts_pre @ [
             Printf.sprintf "int %s = orto_slot_alloc();" slot_v;
             Printf.sprintf "Frame_%s *%s = (Frame_%s*)&ORTO_SLOTS[%s].frame;"
               worker fr_v worker slot_v;
             Printf.sprintf "%s->step = %s_step;" fr_v worker;
             Printf.sprintf "%s->state = 0;" fr_v;
             Printf.sprintf "memset(%s->last_res, 0, sizeof(%s->last_res));" fr_v fr_v;
             Printf.sprintf "memset(%s->return_value, 0, sizeof(%s->return_value));" fr_v fr_v;
             Printf.sprintf "%s->_orto_slot = %s;" fr_v slot_v;
             Printf.sprintf "ORTO_SLOTS[%s].status = ORTO_SLOT_RUNNING;" slot_v;
             Printf.sprintf "ORTO_SLOTS[%s].waiter = NULL;" slot_v;
             Printf.sprintf "long long %s = ORTO_SLOTS[%s].gen;" gen_v slot_v;
           ] @ param_inits @ [
             Printf.sprintf "int %s = %s_step((void*)%s);" rc_v worker fr_v;
             Printf.sprintf "if (%s == 1) ORTO_PENDING++;" rc_v;
             Printf.sprintf "else ORTO_PENDING -= orto_complete((OrtoFrameHeader*)%s);" fr_v;
           ] in
           let task_c = c_type spawn_ty in
           let value = Printf.sprintf "((%s){ .slot = %s, .gen = %s })"
             task_c slot_v gen_v in
           { stmts; value }
       | _ ->
           failwith "emit TESpawn (sync caller): only `spawn worker(args)` \
                     where worker is an async function is supported")
  | Check.T.TEForStream _ ->
      failwith "emit TEForStream: `for x in stream { ... }` only \
                compiles inside an async function — it requires the \
                state-machine lowering."

  | Check.T.TETuple (es, result_ty) ->
      let elem_codes = List.map (emit_expr ctor_map) es in
      let stmts = List.concat_map (fun c -> c.stmts) elem_codes in
      let inits = String.concat ", "
        (List.mapi (fun i c ->
          Printf.sprintf ".f%d = %s" i c.value) elem_codes)
      in
      let value =
        Printf.sprintf "((%s){ %s })" (c_type result_ty) inits
      in
      { stmts; value }

  | Check.T.TETupleIdx (sub, i, _) ->
      let cs = emit_expr ctor_map sub in
      let v = match sub with
        | Check.T.TEVar _ | Check.T.TEFnRef _ ->
            Printf.sprintf "%s.f%d" cs.value i
        | _ ->
            Printf.sprintf "(%s).f%d" cs.value i
      in
      { stmts = cs.stmts; value = v }

  | Check.T.TELetTuple (names, vt, value_e, body, _body_ty, auto_drops) ->
      let cv = emit_expr ctor_map value_e in
      let cb = emit_expr ctor_map body in
      let tmp = fresh "_lt" in
      let comp_tys = match vt with
        | TyTuple ts -> ts
        | _ -> failwith "emit TELetTuple: value not TyTuple"
      in
      let tmp_decl =
        Printf.sprintf "%s %s = %s;" (c_type vt) tmp cv.value
      in
      let binder_decls =
        List.mapi (fun i (n, t) ->
          if n = "_" then
            Printf.sprintf "(void)%s.f%d;" tmp i
          else
            Printf.sprintf "%s %s = %s.f%d;" (c_type t) n tmp i)
          (List.combine names comp_tys)
      in
      (* If any binder has auto_drop=true, after body finishes we run
         drops for the linear components. Materialise body result first. *)
      let needs_drop = List.exists (fun b -> b) auto_drops in
      if needs_drop then
        let body_ty = ty_of_expr body in
        let res = fresh "_lt_res" in
        let res_decl =
          Printf.sprintf "%s %s = %s;" (c_type body_ty) res cb.value
        in
        let drops =
          List.filter_map (fun ((n, t), ad) ->
            if ad && n <> "_" then Some (drop_call_stmt n t)
            else None)
            (List.combine (List.combine names comp_tys) auto_drops)
        in
        let stmts =
          cv.stmts @ [tmp_decl] @ binder_decls @ cb.stmts
          @ [res_decl] @ drops
        in
        { stmts; value = res }
      else
        let stmts =
          cv.stmts @ [tmp_decl] @ binder_decls @ cb.stmts
        in
        { stmts; value = cb.value }

  | Check.T.TEAwaitAll _ ->
      failwith "emit TEAwaitAll: `await all { ... }` only compiles \
                inside an async function — it requires the state-machine \
                lowering."

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
  if e.is_async then begin
    (* `extern async fn f(args) -> T` — source signature is Task[T];
       `extern async stream fn f(args) -> T` — source signature is
       Stream[T]. In both cases the C-side glue takes
       (args..., void *user_data), preps an SQE, and returns int
       (0 on success, < 0 if the ring was full). Single-shot externs
       deliver one CQE; stream externs use IORING_CQE_F_MORE and
       deliver many. *)
    let inner = match e.return_ty with
      | TyApp ("Task", [t]) -> t
      | TyApp ("Stream", [t]) -> t
      | _ ->
          failwith (Printf.sprintf
            "emit: async extern %S has non-Task/Stream return type after check"
            e.name)
    in
    let _ = inner in
    let params_s =
      let user_data = "void *orto_user_data" in
      if e.params = [] then user_data
      else
        String.concat ", "
          ((List.map (fun (x, t) ->
            Printf.sprintf "%s %s" (c_type t) x) e.params)
           @ [user_data])
    in
    Printf.sprintf "extern int %s(%s);" e.name params_s
  end
  else
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

(* ---------- Stage 3 phase 4b/5 — async lowering for `main` ----------

   MVP scope: only `main` may suspend, and the only suspension point
   is `yield`. The runtime spins up an io_uring ring, dispatches CQEs,
   and yield is implemented as a NOP SQE. Frames live in caller stack
   memory for now — a proper slot pool comes with `spawn`. *)

(* Collect every let binding anywhere in the async body. After
   alpha_rename names are globally unique, so we can flatten the whole
   tree into a frame schema without name collisions. We over-collect
   on purpose: locals that never cross a yield still go into the frame
   (a few wasted bytes), but the semantics stay simple — fr->x is the
   canonical home of every named value. *)
let async_collect_locals (body : Check.T.expr) : (string * ty) list =
  let open Check.T in
  let acc = ref [] in
  let add x t = if x <> "_" then acc := (x, t) :: !acc in
  let rec go e =
    match e with
    | TELet (x, t, v, b, _, _) -> add x t; go v; go b
    | TEInt _ | TEFloat _ | TEBool _ | TEStringLit _
    | TEVar _ | TEFnRef _ | TEBreak | TEContinue
    | TENullPtr _ -> ()
    | TECall (f, args, _) -> go f; List.iter go args
    | TEBinop (_, a, b, _) -> go a; go b
    | TEUnop (_, a, _) -> go a
    | TECtor (_, _, args, _) -> List.iter go args
    | TERecord (_, _, fields, _) -> List.iter (fun (_, e) -> go e) fields
    | TEField (e, _, _) -> go e
    | TEIf (c, t, e, _) -> go c; go t; go e
    | TEMatch (s, _, arms, _) ->
        go s;
        List.iter (fun (_, g, b) ->
          (match g with None -> () | Some g -> go g); go b) arms
    | TEArray (r, n, v, _) -> go r; go n; go v
    | TEArrayLit (r, es, _) -> go r; List.iter go es
    | TERegion (n, _) -> go n
    | TEStackRegion (n, _) -> go n
    | TEAlignedRegion (n, a, _) -> go n; go a
    | TEIndex (a, i, _) -> go a; go i
    | TEAssignIdx (a, i, v, _) -> go a; go i; go v
    | TELen (e, _) -> go e
    | TESlice (a, lo, hi, _) -> go a; go lo; go hi
    | TEToInt e | TEToByte e | TEToFloat e | TEToIntFromFloat e
    | TEToU16 e | TEToU32 e | TEToU64 e -> go e
    | TECAlloc (_, n, _) -> go n
    | TECFree e -> go e
    | TEIsNull e -> go e
    | TEArrayData (a, _) -> go a
    | TEDeref (p, _) -> go p
    | TEAssign (_, v, _) -> go v
    | TEWhile (c, b) -> go c; go b
    | TEReturn (v, _) -> go v
    | TETryAt (a, i, _) -> go a; go i
    | TEDrop (e, _) -> go e
    | TEAwait (e, _, _) -> go e
    | TESpawn (e, _) -> go e
    | TEForStream (x, t, s, b) -> add x t; go s; go b
    | TETuple (es, _) -> List.iter go es
    | TETupleIdx (e, _, _) -> go e
    | TELetTuple (ns, vt, v, b, _, _) ->
        let comp_tys = match vt with
          | TyTuple ts -> ts
          | _ -> failwith "async_collect_locals TELetTuple: vt not TyTuple"
        in
        List.iter2 add ns comp_tys;
        go v; go b
    | TEAwaitAll (bs, _, _) -> List.iter go bs
  in
  go body;
  List.rev !acc

(* Rewrite TEVar(x) → TEVar("fr->" ^ x) and TEAssign(x, ...) likewise,
   for every x present in `frame_set`. Names produced by alpha_rename
   are globally unique, so this is just a string-keyed substitution. *)
let async_rewrite_to_frame
  (frame_set : (string, unit) Hashtbl.t)
  (e : Check.T.expr) : Check.T.expr =
  let open Check.T in
  let in_frame x = Hashtbl.mem frame_set x in
  let rename x = if in_frame x then "fr->" ^ x else x in
  let rec go e =
    match e with
    | TEInt _ | TEFloat _ | TEBool _ | TEStringLit _ | TEFnRef _
    | TEBreak | TEContinue | TENullPtr _ -> e
    | TEVar (x, t) -> TEVar (rename x, t)
    | TECall (f, args, t) -> TECall (go f, List.map go args, t)
    | TEBinop (op, a, b, t) -> TEBinop (op, go a, go b, t)
    | TEUnop (op, a, t) -> TEUnop (op, go a, t)
    | TECtor (c, ts, args, t) -> TECtor (c, ts, List.map go args, t)
    | TERecord (n, ts, fields, t) ->
        TERecord (n, ts, List.map (fun (fn, e) -> (fn, go e)) fields, t)
    | TEField (e, fn, t) -> TEField (go e, fn, t)
    | TEIf (c, th, el, t) -> TEIf (go c, go th, go el, t)
    | TELet (x, vt, v, b, bt, ad) ->
        (* Frame-resident binders rewrite to the frame-field name so
           every emission path (inline emit_expr OR the segment
           walker) ends up writing the same fr->x slot. emit_expr's
           TELet case detects the `fr->` prefix and emits an
           assignment instead of a local declaration; the walker's
           store_at avoids double-prefixing the same way. *)
        let x' = if Hashtbl.mem frame_set x then "fr->" ^ x else x in
        TELet (x', vt, go v, go b, bt, ad)
    | TELetTuple (ns, vt, v, b, bt, ads) ->
        let ns' =
          List.map (fun n ->
            if Hashtbl.mem frame_set n then "fr->" ^ n else n) ns
        in
        TELetTuple (ns', vt, go v, go b, bt, ads)
    | TEMatch (s, st, arms, rt) ->
        let arms' =
          List.map (fun (p, g, b) -> (p, Option.map go g, go b)) arms
        in
        TEMatch (go s, st, arms', rt)
    | TEArray (r, n, v, t) -> TEArray (go r, go n, go v, t)
    | TEArrayLit (r, es, t) -> TEArrayLit (go r, List.map go es, t)
    | TERegion (n, t) -> TERegion (go n, t)
    | TEStackRegion (n, t) -> TEStackRegion (go n, t)
    | TEAlignedRegion (n, a, t) -> TEAlignedRegion (go n, go a, t)
    | TEIndex (a, i, t) -> TEIndex (go a, go i, t)
    | TEAssignIdx (a, i, v, t) -> TEAssignIdx (go a, go i, go v, t)
    | TELen (e, t) -> TELen (go e, t)
    | TESlice (a, lo, hi, t) -> TESlice (go a, go lo, go hi, t)
    | TEToInt e -> TEToInt (go e)
    | TEToByte e -> TEToByte (go e)
    | TEToU16 e -> TEToU16 (go e)
    | TEToU32 e -> TEToU32 (go e)
    | TEToU64 e -> TEToU64 (go e)
    | TEToFloat e -> TEToFloat (go e)
    | TEToIntFromFloat e -> TEToIntFromFloat (go e)
    | TECAlloc (et, n, rt) -> TECAlloc (et, go n, rt)
    | TECFree e -> TECFree (go e)
    | TEIsNull e -> TEIsNull (go e)
    | TEArrayData (a, t) -> TEArrayData (go a, t)
    | TEDeref (p, t) -> TEDeref (go p, t)
    | TEAssign (x, v, t) -> TEAssign (rename x, go v, t)
    | TEWhile (c, b) -> TEWhile (go c, go b)
    | TEReturn (v, t) -> TEReturn (go v, t)
    | TETryAt (a, i, t) -> TETryAt (go a, go i, t)
    | TEDrop (e, t) -> TEDrop (go e, t)
    | TEAwait (e, t, p) -> TEAwait (go e, t, p)
    | TESpawn (e, t) -> TESpawn (go e, t)
    | TEForStream (x, et, s, b) ->
        let x' = if Hashtbl.mem frame_set x then "fr->" ^ x else x in
        TEForStream (x', et, go s, go b)
    | TETuple (es, t) -> TETuple (List.map go es, t)
    | TETupleIdx (e, i, t) -> TETupleIdx (go e, i, t)
    | TEAwaitAll (bs, t, ptys) ->
        TEAwaitAll (List.map go bs, t, ptys)
  in go e

(* Does the expression contain a reachable suspension point? Same
   shape as Check.body_has_suspension but exposed for emit's branching
   decisions — when an `if` / `while` / `let` value contains a yield
   or await, we have to split the surrounding code into state segments
   instead of inlining it as plain C. *)
let rec emit_has_suspension (e : Check.T.expr) : bool =
  let open Check.T in
  match e with
  | TEAwait _ -> true
  | TEForStream _ -> true     (* drain loop is a suspension shape *)
  | TESpawn _ -> false        (* spawn launches a child frame *)
  (* break/continue inside async functions must reach the
     surrounding for(;;)switch loop, not the C switch's own break.
     Treat them as "needs splitting" so the walker generates an
     explicit state transition instead of inline-emitting them. *)
  | TEBreak | TEContinue -> true
  | TEInt _ | TEFloat _ | TEBool _ | TEStringLit _
  | TEVar _ | TEFnRef _
  | TENullPtr _ -> false
  | TECall (f, args, _) ->
      emit_has_suspension f || List.exists emit_has_suspension args
  | TEBinop (_, a, b, _) -> emit_has_suspension a || emit_has_suspension b
  | TEUnop (_, a, _) -> emit_has_suspension a
  | TECtor (_, _, args, _) -> List.exists emit_has_suspension args
  | TERecord (_, _, fields, _) ->
      List.exists (fun (_, e) -> emit_has_suspension e) fields
  | TEField (e, _, _) -> emit_has_suspension e
  | TEIf (c, t, e, _) ->
      emit_has_suspension c || emit_has_suspension t || emit_has_suspension e
  | TELet (_, _, v, b, _, _) ->
      emit_has_suspension v || emit_has_suspension b
  | TEMatch (s, _, arms, _) ->
      emit_has_suspension s
      || List.exists (fun (_, g, b) ->
           (match g with None -> false | Some g -> emit_has_suspension g)
           || emit_has_suspension b) arms
  | TEArray (r, n, v, _) ->
      emit_has_suspension r || emit_has_suspension n || emit_has_suspension v
  | TEArrayLit (r, es, _) ->
      emit_has_suspension r || List.exists emit_has_suspension es
  | TERegion (n, _) -> emit_has_suspension n
  | TEStackRegion (n, _) -> emit_has_suspension n
  | TEAlignedRegion (n, a, _) ->
      emit_has_suspension n || emit_has_suspension a
  | TEIndex (a, i, _) -> emit_has_suspension a || emit_has_suspension i
  | TEAssignIdx (a, i, v, _) ->
      emit_has_suspension a || emit_has_suspension i || emit_has_suspension v
  | TELen (e, _) -> emit_has_suspension e
  | TESlice (a, lo, hi, _) ->
      emit_has_suspension a || emit_has_suspension lo || emit_has_suspension hi
  | TEToInt e | TEToByte e | TEToFloat e | TEToIntFromFloat e
  | TEToU16 e | TEToU32 e | TEToU64 e ->
      emit_has_suspension e
  | TECAlloc (_, n, _) -> emit_has_suspension n
  | TECFree e -> emit_has_suspension e
  | TEIsNull e -> emit_has_suspension e
  | TEArrayData (a, _) -> emit_has_suspension a
  | TEDeref (p, _) -> emit_has_suspension p
  | TEAssign (_, v, _) -> emit_has_suspension v
  | TEWhile (c, b) -> emit_has_suspension c || emit_has_suspension b
  | TEReturn (v, _) -> emit_has_suspension v
  | TETryAt (a, i, _) -> emit_has_suspension a || emit_has_suspension i
  | TEDrop (e, _) -> emit_has_suspension e
  | TETuple (es, _) -> List.exists emit_has_suspension es
  | TETupleIdx (e, _, _) -> emit_has_suspension e
  | TELetTuple (_, _, v, b, _, _) ->
      emit_has_suspension v || emit_has_suspension b
  | TEAwaitAll _ -> true

(* Synthetic frame locals required by `await all { ... }` lowerings.
   Reset per function; the walker reads it to know what `fr->...` to
   write each kick's slot/gen into, and `async_collect_locals` reads
   it to add the fields to the frame struct. *)
let await_all_synth_locals : (string * ty) list ref = ref []
let await_all_index : int ref = ref 0
let await_all_walk_index : int ref = ref 0
let dyn_await_index : int ref = ref 0
let dyn_await_walk_index : int ref = ref 0

(* Pre-pass: walks the body, replaces TEAwaitAll(bs, t, ptys) with a
   shape that still carries the same children but is tagged with a
   unique index used by emit to name its synthetic frame locals.  We
   reuse TEAwaitAll itself but invent fresh ordered keys via the
   `await_all_index` counter and add per-branch ints to
   await_all_synth_locals. *)
let allocate_await_all_locals_in_body (body : Check.T.expr) : Check.T.expr =
  let open Check.T in
  let rec go e =
    match e with
    | TEInt _ | TEFloat _ | TEBool _ | TEStringLit _
    | TEVar _ | TEFnRef _ | TEBreak | TEContinue
    | TENullPtr _ -> e
    | TECall (f, args, t) -> TECall (go f, List.map go args, t)
    | TEBinop (op, a, b, t) -> TEBinop (op, go a, go b, t)
    | TEUnop (op, a, t) -> TEUnop (op, go a, t)
    | TECtor (c, ts, args, t) -> TECtor (c, ts, List.map go args, t)
    | TERecord (n, ts, fields, t) ->
        TERecord (n, ts, List.map (fun (fn, e) -> (fn, go e)) fields, t)
    | TEField (e, fn, t) -> TEField (go e, fn, t)
    | TEIf (c, th, el, t) -> TEIf (go c, go th, go el, t)
    | TELet (x, vt, v, b, bt, ad) -> TELet (x, vt, go v, go b, bt, ad)
    | TEMatch (s, st, arms, rt) ->
        TEMatch (go s, st,
          List.map (fun (p, g, b) -> (p, Option.map go g, go b)) arms, rt)
    | TEArray (re, n, v, t) -> TEArray (go re, go n, go v, t)
    | TEArrayLit (re, es, t) -> TEArrayLit (go re, List.map go es, t)
    | TERegion (n, t) -> TERegion (go n, t)
    | TEStackRegion (n, t) -> TEStackRegion (go n, t)
    | TEAlignedRegion (n, a, t) -> TEAlignedRegion (go n, go a, t)
    | TEIndex (a, i, t) -> TEIndex (go a, go i, t)
    | TEAssignIdx (a, i, v, t) -> TEAssignIdx (go a, go i, go v, t)
    | TELen (e, t) -> TELen (go e, t)
    | TESlice (a, lo, hi, t) -> TESlice (go a, go lo, go hi, t)
    | TEToInt e -> TEToInt (go e)
    | TEToByte e -> TEToByte (go e)
    | TEToU16 e -> TEToU16 (go e)
    | TEToU32 e -> TEToU32 (go e)
    | TEToU64 e -> TEToU64 (go e)
    | TEToFloat e -> TEToFloat (go e)
    | TEToIntFromFloat e -> TEToIntFromFloat (go e)
    | TECAlloc (et, n, t) -> TECAlloc (et, go n, t)
    | TECFree e -> TECFree (go e)
    | TEIsNull e -> TEIsNull (go e)
    | TEArrayData (a, t) -> TEArrayData (go a, t)
    | TEDeref (p, t) -> TEDeref (go p, t)
    | TEAssign (x, v, t) -> TEAssign (x, go v, t)
    | TEWhile (c, b) -> TEWhile (go c, go b)
    | TEReturn (v, t) -> TEReturn (go v, t)
    | TETryAt (a, i, t) -> TETryAt (go a, go i, t)
    | TEDrop (e, t) -> TEDrop (go e, t)
    | TEAwait (e, t, p) ->
        (* Dynamic await-all: result type is Array[Result[T]] and the
           inner expression has type Array[Task[T]].  Pre-allocate the
           loop counters and the result-array handle as frame locals so
           they survive across CQE-driven suspensions inside the loop. *)
        (match t with
         | TyApp ("Array", [_]) ->
             let k = !dyn_await_index in
             incr dyn_await_index;
             (* After mono, p is the mono'd inner T; the coll's type
                in the frame is `Array_Task_<pty>`. *)
             let coll_mangled = "Array_Task_" ^ Mono.mangle_ty p in
             let res_ty = t in
             await_all_synth_locals :=
               (Printf.sprintf "_dawn%d_arr" k, TyApp (coll_mangled, [])) ::
               (Printf.sprintf "_dawn%d_res" k, res_ty) ::
               (Printf.sprintf "_dawn%d_i"   k, TyInt) ::
               (Printf.sprintf "_dawn%d_n"   k, TyInt) ::
               !await_all_synth_locals
         | _ -> ());
        TEAwait (go e, t, p)
    | TESpawn (e, t) -> TESpawn (go e, t)
    | TEForStream (x, et, s, b) -> TEForStream (x, et, go s, go b)
    | TETuple (es, t) -> TETuple (List.map go es, t)
    | TETupleIdx (e, i, t) -> TETupleIdx (go e, i, t)
    | TELetTuple (ns, vt, v, b, bt, ads) ->
        TELetTuple (ns, vt, go v, go b, bt, ads)
    | TEAwaitAll (bs, t, ptys) ->
        let k = !await_all_index in
        incr await_all_index;
        let n = List.length bs in
        for i = 0 to n - 1 do
          await_all_synth_locals :=
            (Printf.sprintf "_aw%d_slot_%d" k i, TyInt) ::
            (* 64-bit gen so the counter can't wrap during long-running
               servers. Must match the `long long gen` in OrtoSlot.
               __cll is an emit-internal pseudo-type for `long long`. *)
            (Printf.sprintf "_aw%d_gen_%d"  k i, TyApp ("__cll", [])) ::
            !await_all_synth_locals
        done;
        (* The Result[T] type is built-in but after mono its name is
           the mangled `Result_<pty>` with no args.  Synth locals here
           must match that post-mono shape. *)
        List.iteri (fun i pty ->
          let mangled = "Result_" ^ Mono.mangle_ty pty in
          await_all_synth_locals :=
            (Printf.sprintf "_aw%d_r_%d" k i, TyApp (mangled, []))
            :: !await_all_synth_locals)
          ptys;
        TEAwaitAll (List.map go bs, t, ptys)
  in
  go body

(* Desugar TELetTuple into a chain of TELet bindings.  A fresh tuple-
   typed temporary holds the value; each binder reads tmp.fN.  This
   reuses the existing TELet codepath (auto_drop, async-splitting,
   move-check semantics) so we don't have to duplicate state-machine
   logic for tuple destructuring. *)
let letlet_counter = ref 0
let fresh_letlet () =
  incr letlet_counter;
  Printf.sprintf "_lt_tmp_%d" !letlet_counter

let rec desugar_let_tuples (e : Check.T.expr) : Check.T.expr =
  let open Check.T in
  let r = desugar_let_tuples in
  match e with
  | TEInt _ | TEFloat _ | TEBool _ | TEStringLit _
  | TEVar _ | TEFnRef _ | TEBreak | TEContinue
  | TENullPtr _ -> e
  | TECall (f, args, t) -> TECall (r f, List.map r args, t)
  | TEBinop (op, a, b, t) -> TEBinop (op, r a, r b, t)
  | TEUnop (op, a, t) -> TEUnop (op, r a, t)
  | TECtor (c, ts, args, t) -> TECtor (c, ts, List.map r args, t)
  | TERecord (n, ts, fields, t) ->
      TERecord (n, ts, List.map (fun (fn, e) -> (fn, r e)) fields, t)
  | TEField (e, fn, t) -> TEField (r e, fn, t)
  | TEIf (c, th, el, t) -> TEIf (r c, r th, r el, t)
  | TELet (x, vt, v, b, bt, ad) -> TELet (x, vt, r v, r b, bt, ad)
  | TEMatch (s, st, arms, rt) ->
      TEMatch (r s, st,
        List.map (fun (p, g, b) -> (p, Option.map r g, r b)) arms, rt)
  | TEArray (re, n, v, t) -> TEArray (r re, r n, r v, t)
  | TEArrayLit (re, es, t) -> TEArrayLit (r re, List.map r es, t)
  | TERegion (n, t) -> TERegion (r n, t)
  | TEStackRegion (n, t) -> TEStackRegion (r n, t)
  | TEAlignedRegion (n, a, t) -> TEAlignedRegion (r n, r a, t)
  | TEIndex (a, i, t) -> TEIndex (r a, r i, t)
  | TEAssignIdx (a, i, v, t) -> TEAssignIdx (r a, r i, r v, t)
  | TELen (e, t) -> TELen (r e, t)
  | TESlice (a, lo, hi, t) -> TESlice (r a, r lo, r hi, t)
  | TEToInt e -> TEToInt (r e)
  | TEToByte e -> TEToByte (r e)
  | TEToU16 e -> TEToU16 (r e)
  | TEToU32 e -> TEToU32 (r e)
  | TEToU64 e -> TEToU64 (r e)
  | TEToFloat e -> TEToFloat (r e)
  | TEToIntFromFloat e -> TEToIntFromFloat (r e)
  | TECAlloc (et, n, t) -> TECAlloc (et, r n, t)
  | TECFree e -> TECFree (r e)
  | TEIsNull e -> TEIsNull (r e)
  | TEArrayData (a, t) -> TEArrayData (r a, t)
  | TEDeref (p, t) -> TEDeref (r p, t)
  | TEAssign (x, v, t) -> TEAssign (x, r v, t)
  | TEWhile (c, b) -> TEWhile (r c, r b)
  | TEReturn (v, t) -> TEReturn (r v, t)
  | TETryAt (a, i, t) -> TETryAt (r a, r i, t)
  | TEDrop (e, t) -> TEDrop (r e, t)
  | TEAwait (e, t, p) -> TEAwait (r e, t, p)
  | TESpawn (e, t) -> TESpawn (r e, t)
  | TEForStream (x, et, s, b) -> TEForStream (x, et, r s, r b)
  | TETuple (es, t) -> TETuple (List.map r es, t)
  | TETupleIdx (e, i, t) -> TETupleIdx (r e, i, t)
  | TEAwaitAll (bs, t, ptys) -> TEAwaitAll (List.map r bs, t, ptys)
  | TELetTuple (names, vt, v, b, bt, ads) ->
      let v' = r v in
      let b' = r b in
      let tmp = fresh_letlet () in
      let comp_tys = match vt with
        | TyTuple ts -> ts
        | _ -> failwith "desugar TELetTuple: vt not TyTuple"
      in
      (* Build: let tmp = v; let x = tmp.0; let y = tmp.1; ...; body
         For each binder, auto_drop carries through to its TELet. *)
      let inner =
        List.fold_right (fun ((i, (n, t)), ad) acc ->
          let idx_e = TETupleIdx (TEVar (tmp, vt), i, t) in
          TELet (n, t, idx_e, acc, bt, ad))
          (List.combine
             (List.mapi (fun i p -> (i, p))
                (List.combine names comp_tys))
             ads)
          b'
      in
      (* Underscore binders still need to consume their tuple slot —
         use auto_drop semantics naturally by binding to a fresh
         _drop_N name (check.ml already did this when allocating
         names_actual).  Here `names` already reflects that. *)
      TELet (tmp, vt, v', inner, bt, false)

(* Walk the function body and split into state segments. Each
   suspension point closes the current segment with a real
   io_uring suspend (`return 1`); each control-flow transition
   without suspension closes the current segment with `continue`,
   driving the surrounding `for(;;) switch` loop to the next state
   without leaving the step function. *)
let async_split_segments ctor_map (return_ty : ty) (body : Check.T.expr) : (int * string list) list =
  let open Check.T in
  let segments = ref [] in
  let curr_state = ref 0 in
  let curr_lines = ref [] in
  let segment_open = ref true in
  let next_id = ref 1 in   (* 0 is the entry state *)
  let alloc_state () = let n = !next_id in incr next_id; n in
  let emit_into ss =
    if !segment_open then curr_lines := !curr_lines @ ss
  in
  (* A segment can only be closed once — a `break` inside an `if`
     branch already closes the segment with a jump to the loop's
     exit; a subsequent goto-join from the surrounding if walker
     would create a duplicate `case` for the same state. The open
     flag suppresses that. *)
  let finish_segment stmts =
    if !segment_open then begin
      segments := (!curr_state, !curr_lines @ stmts) :: !segments;
      segment_open := false
    end
  in
  let start_segment id =
    curr_state := id;
    curr_lines := [];
    segment_open := true
  in
  let goto_state next_state =
    [ Printf.sprintf "fr->state = %d;" next_state;
      "continue;" ]
  in
  let async_call_then_state name args next_state =
    let arg_codes = List.map (emit_expr ctor_map) args in
    List.iter (fun cv -> emit_into cv.stmts) arg_codes;
    let arg_values = List.map (fun cv -> cv.value) arg_codes in
    let all_args = arg_values @ ["fr"] in
    emit_into [
      Printf.sprintf "%s(%s);" name (String.concat ", " all_args);
      (* Don't submit here — the dispatcher flushes accumulated SQEs
         in one syscall just before it waits for a CQE. Batching N
         awaits in a row drops N submit-syscalls to one. *)
      "ORTO_NEEDS_SUBMIT = 1;";
      (* Flush if the ring's near full so a long burst of preps\n
         doesn't overflow into NULL sqes. *)
      "if (io_uring_sq_space_left(&ORTO_RING) < 4) { io_uring_submit(&ORTO_RING); ORTO_NEEDS_SUBMIT = 0; }";
    ];
    finish_segment
      [ Printf.sprintf "fr->state = %d;" next_state;
        "return 1;" ]
  in
  let match_async_extern_call (inner : expr) : (string * expr list) option =
    match inner with
    | TECall (TEFnRef (name, _, _), args, _) when is_async_extern name ->
        Some (name, args)
    | _ -> None
  in
  let spawn_counter = ref 0 in
  let fresh_spawn () =
    incr spawn_counter; Printf.sprintf "_sp_%d" !spawn_counter
  in
  let await_counter = ref 0 in
  let fresh_await () =
    incr await_counter; Printf.sprintf "_aw_%d" !await_counter
  in
  (* Emit synchronous frame init + kick for `spawn worker(args)`.
     `detached` selects between joinable (Task survives until awaited)
     and detached (slot self-frees on completion). The resulting Task
     value is returned as a string for the caller to store. *)
  let emit_spawn_setup (worker : string) (args : expr list)
                       (detached : bool) : string * string =
    let arg_codes = List.map (emit_expr ctor_map) args in
    List.iter (fun cv -> emit_into cv.stmts) arg_codes;
    let arg_values = List.map (fun cv -> cv.value) arg_codes in
    let params =
      try Hashtbl.find async_func_params worker
      with Not_found ->
        failwith (Printf.sprintf
          "emit: spawn target %S is not a known async function" worker)
    in
    if List.length params <> List.length arg_values then
      failwith (Printf.sprintf
        "emit: spawn %S: arg/param count mismatch" worker);
    let id = fresh_spawn () in
    let slot_v = id ^ "_slot" in
    let fr_v   = id ^ "_fr" in
    let rc_v   = id ^ "_rc" in
    let gen_v  = id ^ "_gen" in
    let init_status =
      if detached then "ORTO_SLOT_DETACHED" else "ORTO_SLOT_RUNNING"
    in
    let param_inits =
      List.map2 (fun (p, _) v ->
        Printf.sprintf "%s->%s = %s;" fr_v p v) params arg_values
    in
    emit_into ([
      Printf.sprintf "int %s = orto_slot_alloc();" slot_v;
      Printf.sprintf "Frame_%s *%s = (Frame_%s*)&ORTO_SLOTS[%s].frame;"
        worker fr_v worker slot_v;
      Printf.sprintf "%s->step = %s_step;" fr_v worker;
      Printf.sprintf "%s->state = 0;" fr_v;
      Printf.sprintf "memset(%s->last_res, 0, sizeof(%s->last_res));" fr_v fr_v;
      Printf.sprintf "memset(%s->return_value, 0, sizeof(%s->return_value));" fr_v fr_v;
      Printf.sprintf "%s->_orto_slot = %s;" fr_v slot_v;
      Printf.sprintf "%s->more = 0;" fr_v;
      Printf.sprintf "ORTO_SLOTS[%s].status = %s;" slot_v init_status;
      Printf.sprintf "ORTO_SLOTS[%s].waiter = NULL;" slot_v;
      Printf.sprintf "long long %s = ORTO_SLOTS[%s].gen;" gen_v slot_v;
    ] @ param_inits @ [
      Printf.sprintf "int %s = %s_step((void*)%s);" rc_v worker fr_v;
      Printf.sprintf "if (%s == 1) ORTO_PENDING++;" rc_v;
      Printf.sprintf "else ORTO_PENDING -= orto_complete((OrtoFrameHeader*)%s);" fr_v;
    ]);
    (slot_v, gen_v)
  in
  (* Decompose the inner of `TESpawn` to (worker_name, args, is_async).
     `inner` is the bare call expression. A sync target is still
     spawnable — it just runs to completion inline at the spawn site. *)
  let match_spawn_inner (inner : expr) : (string * expr list * bool) option =
    match inner with
    | TECall (TEFnRef (name, _, _), args, _) ->
        Some (name, args, is_async_func name)
    | _ -> None
  in
  (* Sync spawn site: run the call inline, stash the result on the
     slot (joinable) or just discard it (detached). Mirrors the async
     path's contract — `int slot_v, gen_v` get defined so the caller
     can build the Task. *)
  let emit_spawn_sync (worker : string) (args : expr list)
                      (detached : bool) : (string * string) option =
    let arg_codes = List.map (emit_expr ctor_map) args in
    List.iter (fun cv -> emit_into cv.stmts) arg_codes;
    let arg_values = List.map (fun cv -> cv.value) arg_codes in
    let call_expr =
      Printf.sprintf "%s(%s)" worker (String.concat ", " arg_values)
    in
    if detached then begin
      emit_into [Printf.sprintf "(void)%s;" call_expr];
      None
    end else begin
      let id = fresh_spawn () in
      let slot_v = id ^ "_slot" in
      let gen_v  = id ^ "_gen" in
      (* Sync target ran inline; the result is whatever the call
         returned. We don't have its declared return type in this
         path, but the 16-byte slot is big enough for every type the
         language lets escape from a non-async call (int, byte, bool,
         pointer, 8/16-byte struct). memcpy is the uniform answer. *)
      let tmp = fresh "_sync_ret" in
      emit_into [
        Printf.sprintf "int %s = orto_slot_alloc();" slot_v;
        Printf.sprintf "long long %s = ORTO_SLOTS[%s].gen;" gen_v slot_v;
        Printf.sprintf "{ __typeof__(%s) %s = (%s);" call_expr tmp call_expr;
        Printf.sprintf "  memset(ORTO_SLOTS[%s].result, 0, sizeof(ORTO_SLOTS[%s].result));"
          slot_v slot_v;
        Printf.sprintf "  memcpy(ORTO_SLOTS[%s].result, &%s, sizeof(%s)); }"
          slot_v tmp tmp;
        Printf.sprintf "ORTO_SLOTS[%s].status = ORTO_SLOT_DONE_NO_WAITER;" slot_v;
      ];
      Some (slot_v, gen_v)
    end
  in
  (* Detect that the inner of an await is a bare Task[T] variable.
     After alpha_rename + async_rewrite_to_frame, the variable name
     is `fr->t` — we return it verbatim along with T (we need the C
     type of the Task wrapper at the await site). *)
  let match_await_task (inner : expr) : (string * ty) option =
    match inner with
    | TEVar (x, (TyApp ("Task", [t]))) -> Some (x, t)
    | _ -> None
  in
  (* Stack of (continue-state, break-state) for nested async while
     loops, so break/continue inside an async-aware loop body do the
     right state transitions instead of leaking out of the switch. *)
  let loop_stack : (int * int) list ref = ref [] in
  let with_loop head_state exit_state f =
    loop_stack := (head_state, exit_state) :: !loop_stack;
    let r = f () in
    loop_stack := List.tl !loop_stack;
    r
  in
  (* The destination for an expression's value. Carries the destination
     type so we know how to cross the long long ⇄ T boundary at the
     frame header (return_value / last_res are long long; Frame_X
     fields hold T). *)
  let module D = struct
    type t =
      | Local of string * ty   (* fr->name = value; *)
      | Return of ty            (* fr->return_value = value; return 0; *)
      | Discard                 (* statement context; value ignored *)
  end in
  (* Assignment into a 16-byte header slot (return_value/last_res/
     slot.result). Always memcpy from a typed temp — covers scalar T
     (int/byte/bool/float/pointer), 8-byte structs (Region), and
     16-byte aggregates (Array). The slot is zeroed first so reads of
     T smaller than 16 don't pick up stale high bytes. *)
  let assign_to_blob lvalue value_str ty =
    let c_ty = c_type ty in
    let temp = fresh "_blob" in
    [
      Printf.sprintf "memset(&%s, 0, sizeof(%s));" lvalue lvalue;
      Printf.sprintf "{ %s %s = (%s);" c_ty temp value_str;
      Printf.sprintf "  memcpy(&%s, &%s, sizeof(%s)); }" lvalue temp temp;
    ]
  in
  (* Read a T-typed value out of a 16-byte header slot. *)
  let read_from_blob target_lvalue source_lvalue ty =
    let c_ty = c_type ty in
    let temp = fresh "_blob" in
    [
      Printf.sprintf "{ %s %s;" c_ty temp;
      Printf.sprintf "  memcpy(&%s, &%s, sizeof(%s));" temp source_lvalue temp;
      Printf.sprintf "  %s = %s; }" target_lvalue temp;
    ]
  in
  (* async_rewrite_to_frame renames frame-resident binders to "fr->x".
     If that prefix is already there, don't double it; otherwise add
     it (the value lives in the frame either way). *)
  let frame_lvalue name =
    if String.length name > 4 && String.sub name 0 4 = "fr->" then name
    else "fr->" ^ name
  in
  let store_at (d : D.t) value =
    match d with
    | D.Local (x, _) ->
        emit_into [Printf.sprintf "%s = %s;" (frame_lvalue x) value]
    | D.Return ty ->
        finish_segment
          (assign_to_blob "fr->return_value" value ty
           @ ["return 0;"])
    | D.Discard ->
        emit_into [Printf.sprintf "(void)(%s);" value]
  in
  (* Phase 7: wrap the long long in `fr->last_res` into a Result[T]
     value of the dst type. Positive res → Ok((T)res); negative →
     Err((int)(-res)). The constructed wrapper is stored into `dst`.
     `pty` is the payload T (already mono-rewritten). Scalar payloads
     cast directly; non-scalar memcpy through a temp the same way
     read_from_blob does. *)
  let store_await_result (d : D.t) (pty : ty) =
    let result_c = match d with
      | D.Local (_, t) | D.Return t -> c_type t
      | D.Discard ->
          (* No dst, but mono still registered the type — synthesize the
             mangled name from the payload so the wrapper type exists
             even if the value is dropped. *)
          "Result_" ^ Mono.mangle_ty pty
    in
    let tmp = fresh "_res" in
    let check = fresh "_resc" in
    let okv = fresh "_okv" in
    let c_pty = c_type pty in
    let stmts = [
      (* Probe the first 4 bytes of the 16-byte last_res as the
         CQE's int result — that's where the dispatcher writes it. *)
      Printf.sprintf "int %s;" check;
      Printf.sprintf "memcpy(&%s, fr->last_res, sizeof(%s));" check check;
      Printf.sprintf "%s %s;" result_c tmp;
      Printf.sprintf "if (%s >= 0) {" check;
      Printf.sprintf "    %s %s;" c_pty okv;
      Printf.sprintf "    memcpy(&%s, fr->last_res, sizeof(%s));" okv okv;
      Printf.sprintf "    %s = ((%s){ .tag = 0, .as = { .Ok = { .f0 = %s } } });"
        tmp result_c okv;
      "} else {";
      Printf.sprintf "    %s = ((%s){ .tag = 1, .as = { .Err = { .f0 = -%s } } });"
        tmp result_c check;
      "}";
    ] in
    emit_into stmts;
    store_at d tmp
  in
  (* Walk an expression, storing its value into `dst`. If the value
     is produced by a suspension point or branches with suspension,
     this opens new state segments as needed. *)
  let rec walk (dst : D.t) (e : expr) =
    match e with
    | TEAwait (inner, result_ty, pty)
      when (match result_ty with
            | TyApp ("Array", [_]) -> true
            | _ -> false) ->
        (* Dynamic await-all on Array[Task[T]] -> Array[Result[T]].
           The pre-pass allocated frame locals named `_dawn{k}_*`; we
           re-use the same k here via dyn_await_walk_index. *)
        let k = !dyn_await_walk_index in
        incr dyn_await_walk_index;
        let arr_n = Printf.sprintf "_dawn%d_arr" k in
        let res_n = Printf.sprintf "_dawn%d_res" k in
        let i_n   = Printf.sprintf "_dawn%d_i"   k in
        let n_n   = Printf.sprintf "_dawn%d_n"   k in
        let cv = emit_expr ctor_map inner in
        emit_into cv.stmts;
        let result_inner_c = "Result_" ^ Mono.mangle_ty pty in
        let res_arr_c = c_type result_ty in
        let task_c = "Task_" ^ Mono.mangle_ty pty in
        emit_into [
          Printf.sprintf "fr->%s = %s;" arr_n cv.value;
          Printf.sprintf "if (ORTO_REGIONS[fr->%s.slot].gen != fr->%s.expected_gen) abort();"
            arr_n arr_n;
          Printf.sprintf "fr->%s = fr->%s.len;" n_n arr_n;
          Printf.sprintf "if (ORTO_REGIONS[fr->%s.slot].used + (size_t)fr->%s * sizeof(%s) > ORTO_REGIONS[fr->%s.slot].buffer_size) abort();"
            arr_n n_n result_inner_c arr_n;
          Printf.sprintf "{ int _off = (int)ORTO_REGIONS[fr->%s.slot].used;" arr_n;
          Printf.sprintf "  ORTO_REGIONS[fr->%s.slot].used += (size_t)fr->%s * sizeof(%s);"
            arr_n n_n result_inner_c;
          Printf.sprintf "  fr->%s = ((%s){ .slot = fr->%s.slot, .offset = _off, .len = fr->%s, .expected_gen = fr->%s.expected_gen }); }"
            res_n res_arr_c arr_n n_n arr_n;
          Printf.sprintf "fr->%s = 0;" i_n;
        ];
        let head_state = alloc_state () in
        let body_state = alloc_state () in
        let exit_state = alloc_state () in
        finish_segment (goto_state head_state);
        start_segment head_state;
        (* Loop head: if i >= n, exit. Else fetch task at i and either
           deliver inline or hook waiter + suspend. *)
        emit_into [
          Printf.sprintf "if (fr->%s >= fr->%s) { fr->state = %d; continue; }"
            i_n n_n exit_state;
          Printf.sprintf "%s _t = ((%s*)(ORTO_REGIONS[fr->%s.slot].buffer + fr->%s.offset))[fr->%s];"
            task_c task_c arr_n arr_n i_n;
          "if (ORTO_SLOTS[_t.slot].gen != _t.gen) abort();";
          "if (ORTO_SLOTS[_t.slot].status == ORTO_SLOT_DONE_NO_WAITER) {";
          "    memcpy(fr->last_res, ORTO_SLOTS[_t.slot].result, sizeof(fr->last_res));";
          "    orto_slot_free(_t.slot);";
          Printf.sprintf "    fr->state = %d; continue;" body_state;
          "}";
          "ORTO_SLOTS[_t.slot].waiter = (OrtoFrameHeader*)fr;";
          Printf.sprintf "ORTO_SLOTS[_t.slot].waiter_state = %d;" body_state;
        ];
        finish_segment [
          Printf.sprintf "fr->state = %d;" body_state;
          "return 1;";
        ];
        start_segment body_state;
        (* Body: wrap last_res into Result[T] and write into res[i],
           then bump i and loop back to head. last_res is the 16-byte
           header slot (phase 10) — probe int first, memcpy Ok payload. *)
        let tmp = fresh "_dyn" in
        let c_pty = c_type pty in
        let check = fresh "_resc" in
        let okv = fresh "_okv" in
        emit_into [
          Printf.sprintf "%s %s;" result_inner_c tmp;
          Printf.sprintf "int %s;" check;
          Printf.sprintf "memcpy(&%s, fr->last_res, sizeof(%s));" check check;
          Printf.sprintf "if (%s >= 0) {" check;
          Printf.sprintf "    %s %s;" c_pty okv;
          Printf.sprintf "    memcpy(&%s, fr->last_res, sizeof(%s));" okv okv;
          Printf.sprintf "    %s = ((%s){ .tag = 0, .as = { .Ok = { .f0 = %s } } });"
            tmp result_inner_c okv;
          "} else {";
          Printf.sprintf "    %s = ((%s){ .tag = 1, .as = { .Err = { .f0 = -%s } } });"
            tmp result_inner_c check;
          "}";
        ];
        emit_into (
          [
              Printf.sprintf "((%s*)(ORTO_REGIONS[fr->%s.slot].buffer + fr->%s.offset))[fr->%s] = %s;"
                result_inner_c res_n res_n i_n tmp;
              Printf.sprintf "fr->%s = fr->%s + 1;" i_n i_n;
            ]
        );
        finish_segment (goto_state head_state);
        start_segment exit_state;
        store_at dst (Printf.sprintf "fr->%s" res_n)
    | TEAwait (inner, _, pty)
      when (match match_async_extern_call inner with Some _ -> true | None -> false) ->
        let (name, args) = match match_async_extern_call inner with
          | Some r -> r | None -> assert false in
        let n = alloc_state () in
        async_call_then_state name args n;
        start_segment n;
        (* Phase 7: wrap CQE result in Result[T] — Ok(res) if res >= 0,
           else Err(-res) as errno. *)
        store_await_result dst pty
    | TEAwait (inner, _, pty)
      when (match match_await_task inner with Some _ -> true | None -> false) ->
        (* await on a Task[T] variable: check the slot's state. If the
           child already finished synchronously, deliver inline; else
           register us as the waiter and suspend. Either path resumes
           at state N, where the result lives in fr->last_res. *)
        let (task_var, _inner_ty) = match match_await_task inner with
          | Some r -> r | None -> assert false in
        let task_c = c_type (TyApp ("Task", [_inner_ty])) in
        let n = alloc_state () in
        let id = fresh_await () in
        let task_v = id ^ "_task" in
        emit_into [
          Printf.sprintf "%s %s = %s;" task_c task_v task_var;
          Printf.sprintf "if (ORTO_SLOTS[%s.slot].gen != %s.gen) abort();"
            task_v task_v;
          Printf.sprintf "if (ORTO_SLOTS[%s.slot].status == ORTO_SLOT_DONE_NO_WAITER) {"
            task_v;
          Printf.sprintf "    memcpy(fr->last_res, ORTO_SLOTS[%s.slot].result, sizeof(fr->last_res));" task_v;
          Printf.sprintf "    orto_slot_free(%s.slot);" task_v;
          Printf.sprintf "    fr->state = %d;" n;
          Printf.sprintf "    continue;";
          "}";
          Printf.sprintf "ORTO_SLOTS[%s.slot].waiter = (OrtoFrameHeader*)fr;" task_v;
          Printf.sprintf "ORTO_SLOTS[%s.slot].waiter_state = %d;" task_v n;
        ];
        finish_segment [
          Printf.sprintf "fr->state = %d;" n;
          "return 1;";
        ];
        start_segment n;
        (* Phase 7: same Result wrap. Workers don't currently signal
           failure (they just `return` an int), so awaited Task results
           are Ok in practice; the Err path is reserved for the same
           CQE-error shape (negative long long) for shape consistency. *)
        store_await_result dst pty
    | TESpawn (inner, spawn_ty)
      when (match match_spawn_inner inner with Some _ -> true | None -> false) ->
        let (name, args, is_async) = match match_spawn_inner inner with
          | Some r -> r | None -> assert false in
        (* Discard target → detached: slot self-frees on completion;
           Local/Return target → joinable: caller holds a Task. *)
        let detached = (match dst with D.Discard -> true | _ -> false) in
        let slot_pair =
          if is_async then Some (emit_spawn_setup name args detached)
          else emit_spawn_sync name args detached
        in
        (match slot_pair, detached with
         | None, true ->
             (* Detached sync: call was discarded. Emit a no-op value
                so the wrapping store_at D.Discard has something to
                consume. *)
             emit_into ["(void)0;"]
         | None, false ->
             failwith "emit: sync joinable spawn returned no slot"
         | Some (slot_v, gen_v), false ->
             let task_c = c_type spawn_ty in
             let task_lit =
               Printf.sprintf "((%s){ .slot = %s, .gen = %s })"
                 task_c slot_v gen_v
             in
             store_at dst task_lit
         | Some _, true ->
             (* Detached async: emit_spawn_setup already returned a
                slot pair, but we ignore it — detached state self-frees
                on completion. *)
             emit_into ["(void)0;"])
    | TEAwaitAll (branches, result_ty, ptys) ->
        (* Static await-all.  Phase 1: kick every branch concurrently;
           store each kick's (slot, gen) into per-branch frame fields
           so the values survive across the suspensions we'll do in
           phase 2.  Phase 2: sequentially await each sub-slot, write
           the wrapped Result[T] into its frame slot.  Phase 3: build
           the result tuple from the per-branch Result fields. *)
        let k = !await_all_walk_index in
        incr await_all_walk_index;
        let slot_name i = Printf.sprintf "_aw%d_slot_%d" k i in
        let gen_name  i = Printf.sprintf "_aw%d_gen_%d"  k i in
        let r_name    i = Printf.sprintf "_aw%d_r_%d"    k i in
        List.iteri (fun i (br, _pty) ->
          let slot_v = slot_name i in
          let gen_v  = gen_name  i in
          (match br with
           | TECall (TEFnRef (name, _, _), args, _)
               when is_async_extern name ->
               let arg_codes = List.map (emit_expr ctor_map) args in
               List.iter (fun cv -> emit_into cv.stmts) arg_codes;
               let arg_values = List.map (fun cv -> cv.value) arg_codes in
               let sub_fr = Printf.sprintf "_aw%d_sub_%d" k i in
               emit_into [
                 Printf.sprintf "fr->%s = orto_slot_alloc();" slot_v;
                 Printf.sprintf "fr->%s = ORTO_SLOTS[fr->%s].gen;" gen_v slot_v;
                 Printf.sprintf "OrtoFrameHeader *%s = (OrtoFrameHeader*)&ORTO_SLOTS[fr->%s].frame;"
                   sub_fr slot_v;
                 Printf.sprintf "%s->step = orto_passthrough_step;" sub_fr;
                 Printf.sprintf "%s->state = 0;" sub_fr;
                 Printf.sprintf "memset(%s->last_res, 0, sizeof(%s->last_res));" sub_fr sub_fr;
                 Printf.sprintf "memset(%s->return_value, 0, sizeof(%s->return_value));" sub_fr sub_fr;
                 Printf.sprintf "%s->_orto_slot = fr->%s;" sub_fr slot_v;
                 Printf.sprintf "%s->more = 0;" sub_fr;
                 Printf.sprintf "ORTO_SLOTS[fr->%s].status = ORTO_SLOT_RUNNING;" slot_v;
                 Printf.sprintf "ORTO_SLOTS[fr->%s].waiter = NULL;" slot_v;
                 Printf.sprintf "%s(%s);" name
                   (String.concat ", " (arg_values @ [sub_fr]));
                 "ORTO_PENDING++;";
               ]
           | TESpawn (TECall (TEFnRef (worker, _, _), args, _), _) ->
               if is_async_func worker then begin
                 let (s, g) = emit_spawn_setup worker args false in
                 emit_into [
                   Printf.sprintf "fr->%s = %s;" slot_v s;
                   Printf.sprintf "fr->%s = %s;" gen_v g;
                 ]
               end else begin
                 match emit_spawn_sync worker args false with
                 | Some (s, g) ->
                     emit_into [
                       Printf.sprintf "fr->%s = %s;" slot_v s;
                       Printf.sprintf "fr->%s = %s;" gen_v g;
                     ]
                 | None ->
                     failwith "await all: sync joinable spawn returned no slot"
               end
           | _ ->
               failwith
                 "await all: each branch must be an `extern async` call \
                  or `spawn worker(args)`"))
          (List.combine branches ptys);
        (* Mark SQEs ready; dispatcher submits before its next wait. *)
        emit_into ["ORTO_NEEDS_SUBMIT = 1;"];
        (* Phase 2: sequentially await each sub-slot. *)
        List.iteri (fun i pty ->
          let slot_v = slot_name i in
          let gen_v  = gen_name  i in
          let r_v    = r_name    i in
          let n_state = alloc_state () in
          emit_into [
            Printf.sprintf "if (ORTO_SLOTS[fr->%s].gen != fr->%s) abort();"
              slot_v gen_v;
            Printf.sprintf
              "if (ORTO_SLOTS[fr->%s].status == ORTO_SLOT_DONE_NO_WAITER) {"
              slot_v;
            Printf.sprintf "    memcpy(fr->last_res, ORTO_SLOTS[fr->%s].result, sizeof(fr->last_res));" slot_v;
            Printf.sprintf "    orto_slot_free(fr->%s);" slot_v;
            Printf.sprintf "    fr->state = %d;" n_state;
            Printf.sprintf "    continue;";
            "}";
            Printf.sprintf
              "ORTO_SLOTS[fr->%s].waiter = (OrtoFrameHeader*)fr;" slot_v;
            Printf.sprintf
              "ORTO_SLOTS[fr->%s].waiter_state = %d;" slot_v n_state;
          ];
          finish_segment [
            Printf.sprintf "fr->state = %d;" n_state;
            "return 1;";
          ];
          start_segment n_state;
          (* last_res is now uint8_t[16] (phase 10). Probe the first
             4 bytes as the CQE int result, build Ok/Err from there
             with memcpy for the Ok payload (covers Region, Array,
             nested-tuple etc.). Mirrors store_await_result. *)
          let result_c = "Result_" ^ Mono.mangle_ty pty in
          let c_pty = c_type pty in
          let check = fresh "_resc" in
          let okv = fresh "_okv" in
          let stmts = [
            Printf.sprintf "int %s;" check;
            Printf.sprintf "memcpy(&%s, fr->last_res, sizeof(%s));" check check;
            Printf.sprintf "if (%s >= 0) {" check;
            Printf.sprintf "    %s %s;" c_pty okv;
            Printf.sprintf "    memcpy(&%s, fr->last_res, sizeof(%s));" okv okv;
            Printf.sprintf "    fr->%s = ((%s){ .tag = 0, .as = { .Ok = { .f0 = %s } } });"
              r_v result_c okv;
            "} else {";
            Printf.sprintf "    fr->%s = ((%s){ .tag = 1, .as = { .Err = { .f0 = -%s } } });"
              r_v result_c check;
            "}";
          ] in
          emit_into stmts
        ) ptys;
        (* Phase 3: build tuple from the per-branch Result fields. *)
        let inits = String.concat ", "
          (List.mapi (fun i _ ->
            Printf.sprintf ".f%d = fr->%s" i (r_name i)) ptys)
        in
        let tuple_lit =
          Printf.sprintf "((%s){ %s })" (c_type result_ty) inits
        in
        store_at dst tuple_lit
    | TELet (x, ty, v, b, _, _) ->
        (* Bind v, then walk b. v may suspend — walk recursively
           with destination = the binder. *)
        let v_dst =
          if x = "_" then D.Discard else D.Local (x, ty)
        in
        walk v_dst v;
        walk dst b
    | TEIf (cond, t, e_br, _) when emit_has_suspension t || emit_has_suspension e_br ->
        let cv = emit_expr ctor_map cond in
        emit_into cv.stmts;
        let t_state = alloc_state () in
        let e_state = alloc_state () in
        let join = match dst with
          | D.Return _ -> -1   (* both branches terminate by return *)
          | _ -> alloc_state ()
        in
        finish_segment [
          Printf.sprintf "if (%s) { fr->state = %d; continue; }" cv.value t_state;
          Printf.sprintf "fr->state = %d;" e_state;
          "continue;";
        ];
        start_segment t_state;
        walk dst t;
        if join >= 0 then finish_segment (goto_state join);
        start_segment e_state;
        walk dst e_br;
        if join >= 0 then finish_segment (goto_state join);
        if join >= 0 then start_segment join
    | TEForStream (x, et, src, body_e) ->
        (* Multishot drain. Two-shape source:
             stream_extern(args)  — inline: prep the multishot SQE
                                    here, then HEAD waits on CQEs.
             <stream-var>         — bound: not yet supported. The
                                    let-binding form has nowhere to
                                    prep the SQE in the current
                                    pipeline; deferred to a follow-up.
           For each CQE: write fr->last_res into the binder, run the
           body, then either loop back (fr->more == 1) or exit
           (fr->more == 0, which io_uring sets on the final CQE). *)
        let (extern_name, args) = match src with
          | TECall (TEFnRef (n, _, _), args, _) when is_async_extern n && is_stream_extern n ->
              (n, args)
          | TECall (TEFnRef (n, _, _), _, _) when is_async_extern n ->
              failwith (Printf.sprintf
                "emit for-stream: %S is not declared as a stream extern \
                 (use `extern async stream fn ...`)" n)
          | TEVar (_, _) ->
              failwith "emit for-stream: bound Stream variables are not \
                        yet supported — inline the call: \
                        `for x in stream_extern(...) { ... }`"
          | _ ->
              failwith "emit for-stream: source must be a stream-extern \
                        call expression"
        in
        let arg_codes = List.map (emit_expr ctor_map) args in
        List.iter (fun cv -> emit_into cv.stmts) arg_codes;
        let arg_values = List.map (fun cv -> cv.value) arg_codes in
        let all_args = arg_values @ ["fr"] in
        let head_s = alloc_state () in
        let exit_s = alloc_state () in
        emit_into [
          Printf.sprintf "%s(%s);" extern_name (String.concat ", " all_args);
          "ORTO_NEEDS_SUBMIT = 1;";
      (* Flush if the ring's near full so a long burst of preps\n
         doesn't overflow into NULL sqes. *)
      "if (io_uring_sq_space_left(&ORTO_RING) < 4) { io_uring_submit(&ORTO_RING); ORTO_NEEDS_SUBMIT = 0; }";
        ];
        finish_segment [
          Printf.sprintf "fr->state = %d;" head_s;
          "return 1;";
        ];
        start_segment head_s;
        (* Each CQE delivered by the dispatcher is an event the body
           wants to see — even the final one (multishot ops set more=0
           on the last CQE but the payload is still meaningful, e.g.
           the last accepted fd before the source closed). Decode
           last_res into the binder and run the body unconditionally;
           the user can `break` on a negative payload if they want
           per-event error handling.
           Phase 7 asymmetry: `await Task[T]` wraps in Result[T], but
           stream events stay raw. Wrapping every multishot event would
           cost an allocation per CQE and would change the established
           shape `for x in accept_stream(...) { if x < 0 { break } ... }`
           without giving anything that the inline check doesn't. *)
        if x <> "_" then
          emit_into (read_from_blob (frame_lvalue x) "fr->last_res" et);
        with_loop head_s exit_s (fun () ->
          walk D.Discard body_e);
        (* After body: if no more CQEs are coming, leave; else wait
           for the next. We don't submit a new SQE — multishot drives
           further CQEs by itself. *)
        finish_segment [
          Printf.sprintf "if (!fr->more) { fr->state = %d; continue; }" exit_s;
          Printf.sprintf "fr->state = %d;" head_s;
          "return 1;";
        ];
        start_segment exit_s;
        store_at dst "0"
    | TEWhile (cond, body_e) when emit_has_suspension cond || emit_has_suspension body_e ->
        let head = alloc_state () in
        let body_s = alloc_state () in
        let exit_s = alloc_state () in
        finish_segment (goto_state head);
        start_segment head;
        let cv = emit_expr ctor_map cond in
        emit_into cv.stmts;
        finish_segment [
          Printf.sprintf "if (%s) { fr->state = %d; continue; }" cv.value body_s;
          Printf.sprintf "fr->state = %d;" exit_s;
          "continue;";
        ];
        start_segment body_s;
        with_loop head exit_s (fun () ->
          walk D.Discard body_e);
        finish_segment (goto_state head);
        start_segment exit_s;
        store_at dst "0"   (* while as expression evaluates to int 0 *)
    | TEBreak when !loop_stack <> [] ->
        let (_, exit_s) = List.hd !loop_stack in
        finish_segment (goto_state exit_s)
    | TEContinue when !loop_stack <> [] ->
        let (head_s, _) = List.hd !loop_stack in
        finish_segment (goto_state head_s)
    | TEReturn (v, ty) ->
        walk (D.Return ty) v
    | _ when not (emit_has_suspension e) ->
        let cv = emit_expr ctor_map e in
        emit_into cv.stmts;
        store_at dst cv.value
    | _ ->
        (* Suspension inside an expression form we don't yet split
           (e.g. inside a function call argument, a struct literal,
           etc.). The user can lift such expressions into a let to
           sequence the suspension cleanly. *)
        failwith
          ("emit async: suspension inside an expression form that's \
             not yet supported for splitting. Lift the awaiting call \
             into a `let` first.")
  in
  walk (D.Return return_ty) body;
  List.rev !segments

(* Emit a Frame struct for an async function. Layout matches
   OrtoFrameHeader exactly for the first five fields so the dispatcher
   can read them generically through a header pointer:
     step, state, last_res, return_value, _orto_slot
   `_orto_slot` is the index of the slot this frame lives in, or -1
   if the frame lives on the caller's C stack (the sync wrapper
   path — `main` and the top of a sync->async call). *)
let emit_async_frame_struct (f : Check.T.func) : string =
  letlet_counter := 0;
  await_all_index := 0;
  dyn_await_index := 0;
  await_all_synth_locals := [];
  let desugared0 = desugar_let_tuples f.body in
  let desugared = allocate_await_all_locals_in_body desugared0 in
  let synth_locals = List.rev !await_all_synth_locals in
  let locals = async_collect_locals desugared @ synth_locals in
  let params_lines =
    List.map (fun (p, t) ->
      Printf.sprintf "    %s %s;" (c_type t) p) f.params
  in
  let local_lines =
    List.map (fun (x, t) ->
      Printf.sprintf "    %s %s;" (c_type t) x) locals
  in
  (* Must match OrtoFrameHeader exactly — the dispatcher and
     orto_complete cast Frame_<X>* to OrtoFrameHeader* to read these. *)
  let header = [
    "    OrtoStepFn step;";
    "    int state;";
    "    int _orto_slot;";          (* slot index, or -1 if frame is stack-owned *)
    "    int more;";                (* multishot: 1 if IORING_CQE_F_MORE set on last CQE *)
    "    uint8_t last_res[16];";    (* dispatcher writes the CQE result here before resume *)
    "    uint8_t return_value[16];"; (* step writes this before `return 0` *)
  ] in
  Printf.sprintf "typedef struct {\n%s\n} Frame_%s;"
    (String.concat "\n" (header @ params_lines @ local_lines))
    f.name

(* Emit the step function for an async function. *)
let emit_async_step ctor_map (f : Check.T.func) : string =
  reset_counter ();
  letlet_counter := 0;
  await_all_index := 0;
  dyn_await_index := 0;
  await_all_synth_locals := [];
  let desugared0 = desugar_let_tuples f.body in
  let desugared = allocate_await_all_locals_in_body desugared0 in
  let synth_locals = List.rev !await_all_synth_locals in
  let locals = async_collect_locals desugared @ synth_locals in
  let frame_set = Hashtbl.create (List.length locals + List.length f.params) in
  List.iter (fun (x, _) -> Hashtbl.add frame_set x ()) locals;
  List.iter (fun (p, _) -> Hashtbl.add frame_set p ()) f.params;
  let body' = async_rewrite_to_frame frame_set desugared in
  await_all_walk_index := 0;
  dyn_await_walk_index := 0;
  let segments = async_split_segments ctor_map f.return_ty body' in
  let case_lines =
    List.concat_map (fun (state, lines) ->
      let indented = List.map (fun s -> "        " ^ s) lines in
      (Printf.sprintf "    case %d: {" state)
      :: indented
      @ ["    }"]
    ) segments
  in
  Printf.sprintf
    "static int %s_step(void *frp) {\n\
     \    Frame_%s *fr = (Frame_%s *)frp;\n\
     \    for (;;) switch (fr->state) {\n\
     %s\n\
     \    default: return 0;\n\
     \    }\n\
     }"
    f.name f.name f.name
    (String.concat "\n" case_lines)

(* Emit `main` as a C-level wrapper that owns the ring, allocates the
   frame on the stack, kicks state 0, and drains the dispatcher. *)
(* Per-thread body — runs once on each worker (or once total when
   cores=1). Returns `result` via the local variable; the cores=1
   `main` returns it as int, cores>1 thread fn casts to void*. *)
let emit_async_main_wrapper (f : Check.T.func) : string =
  let body init_fail_return =
    Printf.sprintf
      "    int result = 0;\n\
       \    if (io_uring_queue_init(ORTO_RING_ENTRIES, &ORTO_RING, 0) < 0) {\n\
       \        result = 1;\n\
       \        %s;\n\
       \    }\n\
       \    orto_init_regions();\n\
       \    orto_init_slots();\n\
       \    Frame_%s fr;\n\
       \    fr.step = %s_step;\n\
       \    fr.state = 0;\n\
       \    memset(fr.last_res, 0, sizeof(fr.last_res));\n\
       \    memset(fr.return_value, 0, sizeof(fr.return_value));\n\
       \    fr._orto_slot = -1;\n\
       \    fr.more = 0;\n\
       \    ORTO_PENDING = 1;\n\
       \    int rc = %s_step(&fr);\n\
       \    if (rc == 0) ORTO_PENDING = 0;\n\
       \    else orto_dispatch();\n\
       \    memcpy(&result, &fr.return_value, sizeof(result));\n\
       \    io_uring_queue_exit(&ORTO_RING);\n"
      init_fail_return f.name f.name f.name
  in
  Printf.sprintf
    "#if ORTO_CORES > 1\n\
     static void *%s_thread(void *_arg) {\n\
     \    (void)_arg;\n\
     %s\
     \    return (void *)(long)result;\n\
     }\n\
     int %s(void) {\n\
     \    pthread_t _ts[ORTO_CORES];\n\
     \    for (int i = 0; i < ORTO_CORES; i++)\n\
     \        pthread_create(&_ts[i], NULL, %s_thread, NULL);\n\
     \    void *_r0 = NULL;\n\
     \    for (int i = 0; i < ORTO_CORES; i++) {\n\
     \        void *_r;\n\
     \        pthread_join(_ts[i], &_r);\n\
     \        if (i == 0) _r0 = _r;\n\
     \    }\n\
     \    return (int)(long)_r0;\n\
     }\n\
     #else\n\
     int %s(void) {\n\
     %s\
     \    return result;\n\
     }\n\
     #endif"
    f.name (body "return (void *)(long)result") f.name f.name
    f.name (body "return result")

(* Emit a sync C wrapper for a non-main async function. It builds the
   frame on the C stack with _orto_slot = -1, runs the step, and if the
   step suspends, drains the local dispatcher. Returns the unwrapped int
   so sync callers can use an async function without ever seeing Task.
   The function's source-visible signature is `fn name(params) -> int`,
   so we just emit it as such. *)
let emit_async_sync_wrapper (f : Check.T.func) : string =
  let params_s =
    if f.params = [] then "void"
    else
      String.concat ", "
        (List.map (fun (x, t) ->
          Printf.sprintf "%s %s" (c_type t) x) f.params)
  in
  let param_inits =
    List.map (fun (p, _) ->
      Printf.sprintf "    fr.%s = %s;" p p) f.params
  in
  let return_stmt =
    Printf.sprintf
      "    %s _ret;\n\
       \    memcpy(&_ret, &fr.return_value, sizeof(_ret));\n\
       \    return _ret;"
      (c_type f.return_ty)
  in
  Printf.sprintf
    "%s %s(%s) {\n\
     \    Frame_%s fr;\n\
     \    fr.step = %s_step;\n\
     \    fr.state = 0;\n\
     \    memset(fr.last_res, 0, sizeof(fr.last_res));\n\
     \    memset(fr.return_value, 0, sizeof(fr.return_value));\n\
     \    fr._orto_slot = -1;\n\
     \    fr.more = 0;\n\
     %s\n\
     \    int rc = %s_step(&fr);\n\
     \    if (rc == 1) {\n\
     \        ORTO_PENDING++;\n\
     \        orto_dispatch();\n\
     \    }\n\
     %s\n\
     }"
    (c_type f.return_ty) f.name params_s
    f.name f.name
    (String.concat "\n" param_inits)
    f.name
    return_stmt

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

let emit ?(slots=1024) ?(cores=1) ?(ring_entries=64) (prog : Check.T.program) : string =
  let prog =
    { prog with funcs = List.map alpha_rename_func prog.funcs }
  in
  collect_program prog;
  let ctor_map = build_ctor_map prog.types in
  let adt_forwards = List.map emit_adt_forward prog.types in
  let rec_forwards = List.map emit_record_forward prog.records in
  let array_forwards = emit_array_forwards () in
  let task_forwards = emit_task_forwards () in
  let tuple_forwards = emit_tuple_forwards () in
  let tuple_drop_forwards = emit_tuple_drop_forwards () in
  let tuple_drop_defs = emit_tuple_drop_defs () in
  let array_drop_forwards = emit_array_drop_forwards () in
  let async_funcs =
    List.filter (fun (f : Check.T.func) -> f.is_async) prog.funcs
  in
  (* drop_Task / drop_Stream reference ORTO_SLOTS, which only exists
     when the async runtime is emitted. A fully-sync program may
     still have Task[T] in its type universe (orto_nop builtin),
     but it never instantiates one, so the drop helpers aren't
     needed — and if emitted, they'd fail to compile against the
     missing slot pool. Skip them in sync mode. *)
  let has_async = async_funcs <> [] in
  let task_drop_forwards = if has_async then emit_task_drop_forwards () else [] in
  let task_drop_defs     = if has_async then emit_task_drop_defs ()     else [] in
  let stream_drop_forwards = if has_async then emit_stream_drop_forwards () else [] in
  let stream_drop_defs     = if has_async then emit_stream_drop_defs ()     else [] in
  let fn_typedefs  = emit_fn_typedefs () in
  let ordered_structs = topo_sort_structs prog.types prog.records in
  let struct_defs = List.map (function
    | DAdt td -> emit_adt_definition td
    | DRec rd -> emit_record_definition rd) ordered_structs in
  let extern_decls = List.map emit_extern_decl prog.externs in
  let sync_funcs =
    List.filter (fun (f : Check.T.func) -> not f.is_async) prog.funcs
  in
  let decls        = List.map emit_func_decl sync_funcs in
  (* Non-main async functions also expose a sync C symbol (forward
     decl matches the sync wrapper's signature: `T name(params)`). *)
  let async_decls_sync =
    List.filter_map (fun (f : Check.T.func) ->
      if f.name = "main" then None
      else Some (emit_func_decl f)) async_funcs
  in
  let array_drop_defs = emit_array_drop_defs () in
  let defs         = List.map (emit_func_def ctor_map) sync_funcs in
  let async_runtime, async_decls, async_defs =
    if async_funcs = [] then ("", [], [])
    else begin
      (* A union over every Frame_<X> sized so the slot's inline byte
         buffer fits the largest async frame in the program. We don't
         have to do the sizeof math in OCaml — C does it. *)
      let frame_union =
        if async_funcs = [] then ""
        else
          let fields =
            List.mapi (fun i (f : Check.T.func) ->
              Printf.sprintf "    Frame_%s f%d;" f.name i) async_funcs
          in
          Printf.sprintf
            "typedef union {\n%s\n} OrtoSlotFrames;"
            (String.concat "\n" fields)
      in
      let rt =
        Printf.sprintf
          "/* Stage 3 async runtime: an io_uring ring, a tiny CQE-driven\n\
           \ * dispatcher, helpers for the lowered state machines, and a\n\
           \ * fixed-slab slot pool that owns every spawned frame.\n\
           \ *\n\
           \ * The header layout below matches the prefix of every\n\
           \ * Frame_<X> struct (step / state / last_res / return_value\n\
           \ * / _orto_slot), so the dispatcher reads and writes those\n\
           \ * fields generically through an OrtoFrameHeader pointer. */\n\
           #include <liburing.h>\n\
           #include <string.h>\n\
           /* The header lives at the prefix of every Frame_<X>. We use\n\
            * long long for last_res and return_value so any T up to 8\n\
            * bytes (int, byte, bool, Region, Task, Stream, raw pointer,\n\
            * double via bitcast, Region's 8-byte handle, Array's\n\
            * 16-byte handle, every 2-/3-/4-component tuple of those)\n\
            * can travel through the slot pool without per-T machinery.\n\
            * Wider returns must currently be Region-boxed. */\n\
           typedef struct {\n\
           \    OrtoStepFn step;\n\
           \    int state;\n\
           \    int _orto_slot;   /* slot index in ORTO_SLOTS, -1 if stack-owned */\n\
           \    int more;         /* multishot: 1 if more CQEs are coming, 0 on EOF */\n\
           \    uint8_t last_res[16];\n\
           \    uint8_t return_value[16];\n\
           } OrtoFrameHeader;\n\
           /* ORTO_RING is intentionally non-static so user-side\n\
            * async-extern glue (extern fn read/write/recv/...) can\n\
            * submit SQEs directly into the same ring. With cores>1\n\
            * it's also __thread — glue must use\n\
            * `extern __thread struct io_uring ORTO_RING;` then. */\n\
           ORTO_TLS struct io_uring ORTO_RING;\n\
           ORTO_TLS int ORTO_PENDING = 0;\n\
           /* Set by every SQE-prep emit site; cleared by the\n\
            * dispatcher right after it flushes them. Lets us coalesce\n\
            * N back-to-back preps into one io_uring_submit syscall. */\n\
           ORTO_TLS int ORTO_NEEDS_SUBMIT = 0;\n\
           \n\
           %s\n\
           \n\
           /* Slot pool — TigerBeetle-style fixed slab. Spawned frames\n\
            * live inside `.frame`; ORTO_SLOT_FREE marks a free entry,\n\
            * ORTO_SLOT_RUNNING a joinable frame still running,\n\
            * ORTO_SLOT_DETACHED a fire-and-forget frame (drop slot on\n\
            * completion), ORTO_SLOT_DONE_NO_WAITER a finished joinable\n\
            * frame whose result hasn't been picked up yet. */\n\
           /* Slot pool size, set at compile time by orto's --slots N\n\
            * flag (default 1024). All concurrent tasks live in this\n\
            * pool; raise it if you spawn more than ORTO_SLOT_COUNT\n\
            * tasks in flight. No dynamic growth in v1 (slab list +\n\
            * stable pointers is a v2 follow-up). */\n\
           #define ORTO_SLOT_COUNT %d\n\
           #define ORTO_SLOT_FREE 0\n\
           #define ORTO_SLOT_RUNNING 1\n\
           #define ORTO_SLOT_DETACHED 2\n\
           #define ORTO_SLOT_DONE_NO_WAITER 3\n\
           typedef struct {\n\
           \    int gen;\n\
           \    int next_free;\n\
           \    int status;\n\
           \    int waiter_state;\n\
           \    uint8_t result[16];\n\
           \    OrtoFrameHeader *waiter;\n\
           \    OrtoSlotFrames frame;\n\
           } OrtoSlot;\n\
           ORTO_TLS OrtoSlot ORTO_SLOTS[ORTO_SLOT_COUNT];\n\
           ORTO_TLS int ORTO_SLOT_FREE_HEAD = -1;\n\
           \n\
           static int orto_slot_alloc(void) {\n\
           \    if (ORTO_SLOT_FREE_HEAD < 0) abort();\n\
           \    int i = ORTO_SLOT_FREE_HEAD;\n\
           \    ORTO_SLOT_FREE_HEAD = ORTO_SLOTS[i].next_free;\n\
           \    ORTO_SLOTS[i].next_free = -1;\n\
           \    ORTO_SLOTS[i].status = ORTO_SLOT_RUNNING;\n\
           \    ORTO_SLOTS[i].waiter = NULL;\n\
           \    ORTO_SLOTS[i].waiter_state = 0;\n\
           \    memset(ORTO_SLOTS[i].result, 0, sizeof(ORTO_SLOTS[i].result));\n\
           \    return i;\n\
           }\n\
           \n\
           static void orto_slot_free(int i) {\n\
           \    ORTO_SLOTS[i].gen++;  /* invalidate dangling Task handles */\n\
           \    ORTO_SLOTS[i].status = ORTO_SLOT_FREE;\n\
           \    ORTO_SLOTS[i].waiter = NULL;\n\
           \    ORTO_SLOTS[i].next_free = ORTO_SLOT_FREE_HEAD;\n\
           \    ORTO_SLOT_FREE_HEAD = i;\n\
           }\n\
           \n\
           /* A frame just returned 0 from its step. Either deliver the\n\
            * result to the waiter (and re-enter it, possibly chaining)\n\
            * or stash it in the slot for a later await. Detached frames\n\
            * just release their slot. Returns 0 if no further pending\n\
            * adjustment is needed beyond the caller's own --, or the\n\
            * number of extra `ORTO_PENDING--`s the caller should apply\n\
            * because re-entered waiters also completed.\n\
            *\n\
            * Re-entry is a loop, not recursion, so an arbitrarily long\n\
            * await-of-await chain stays O(1) on the C stack. */\n\
           static int orto_complete(OrtoFrameHeader *fr) {\n\
           \    int extra_done = 0;\n\
           \    for (;;) {\n\
           \        if (fr->_orto_slot < 0) return extra_done;\n\
           \        int si = fr->_orto_slot;\n\
           \        OrtoSlot *s = &ORTO_SLOTS[si];\n\
           \        memcpy(&s->result, &fr->return_value, sizeof(s->result));\n\
           \        if (s->status == ORTO_SLOT_DETACHED) {\n\
           \            orto_slot_free(si);\n\
           \            return extra_done;\n\
           \        }\n\
           \        if (s->waiter == NULL) {\n\
           \            s->status = ORTO_SLOT_DONE_NO_WAITER;\n\
           \            return extra_done;\n\
           \        }\n\
           \        OrtoFrameHeader *w = s->waiter;\n\
           \        memcpy(&w->last_res, &fr->return_value, sizeof(w->last_res));\n\
           \        w->state = s->waiter_state;\n\
           \        orto_slot_free(si);\n\
           \        int wr = w->step(w);\n\
           \        if (wr == 1) return extra_done;  /* waiter still pending */\n\
           \        extra_done++;\n\
           \        fr = w;  /* chain: deliver this waiter's result, too */\n\
           \    }\n\
           }\n\
           \n\
           /* Built-in async extern that backs the `yield` keyword.\n\
            * The parser desugars `yield` to `await orto_nop()`, so\n\
            * the rest of the lowering routes through the ordinary\n\
            * async-extern await path. The caller emits the submit\n\
            * after this returns. */\n\
           int orto_nop(void *fr) {\n\
           \    struct io_uring_sqe *sqe = io_uring_get_sqe(&ORTO_RING);\n\
           \    io_uring_prep_nop(sqe);\n\
           \    io_uring_sqe_set_data(sqe, fr);\n\
           \    return 0;\n\
           }\n\
           \n\
           /* Single iteration: handle one CQE. Factored so we can\n\
            * drain a whole batch in one pass without syscalls. */\n\
           static inline void orto_handle_cqe(struct io_uring_cqe *cqe) {\n\
           \    OrtoFrameHeader *fr = io_uring_cqe_get_data(cqe);\n\
           \    memset(&fr->last_res, 0, sizeof(fr->last_res));\n\
           \    memcpy(&fr->last_res, &cqe->res, sizeof(cqe->res));\n\
           \    fr->more = (cqe->flags & IORING_CQE_F_MORE) ? 1 : 0;\n\
           \    io_uring_cqe_seen(&ORTO_RING, cqe);\n\
           \    int sr = fr->step(fr);\n\
           \    if (sr == 0) {\n\
           \        ORTO_PENDING--;\n\
           \        ORTO_PENDING -= orto_complete(fr);\n\
           \    }\n\
           }\n\
           \n\
           static int orto_dispatch(void) {\n\
           \    while (ORTO_PENDING > 0) {\n\
           \        if (ORTO_NEEDS_SUBMIT) {\n\
           \            io_uring_submit(&ORTO_RING);\n\
           \            ORTO_NEEDS_SUBMIT = 0;\n\
           \        }\n\
           \        struct io_uring_cqe *cqe;\n\
           \        int rc = io_uring_wait_cqe(&ORTO_RING, &cqe);\n\
           \        if (rc < 0) return rc;\n\
           \        orto_handle_cqe(cqe);\n\
           \        /* Drain everything else ready in userspace — no\n\
           \         * extra syscalls; halves syscall count for any\n\
           \         * fan-out workload. */\n\
           \        while (ORTO_PENDING > 0) {\n\
           \            if (io_uring_peek_cqe(&ORTO_RING, &cqe) != 0) break;\n\
           \            orto_handle_cqe(cqe);\n\
           \        }\n\
           \    }\n\
           \    return 0;\n\
           }\n\
           \n\
           /* Passthrough step for sub-slots in `await all { ... }`.\n\
            * The dispatcher writes the CQE result into fr->last_res\n\
            * and then calls step(fr). We copy the 16-byte slot to\n\
            * return_value and return 0 — orto_complete then delivers\n\
            * to whoever is waiting on this slot. */\n\
           static int orto_passthrough_step(void *frp) {\n\
           \    OrtoFrameHeader *fr = (OrtoFrameHeader*)frp;\n\
           \    memcpy(&fr->return_value, &fr->last_res, sizeof(fr->return_value));\n\
           \    return 0;\n\
           }\n\
           \n\
           /* Called once per thread (main wrapper invokes it). Was\n\
            * a constructor in the single-thread era — that doesn't\n\
            * work with TLS, where every thread has its own pool. */\n\
           static void orto_init_slots(void) {\n\
           \    for (int i = 0; i < ORTO_SLOT_COUNT; i++) {\n\
           \        ORTO_SLOTS[i].gen = 1;\n\
           \        ORTO_SLOTS[i].next_free = i + 1;\n\
           \        ORTO_SLOTS[i].status = ORTO_SLOT_FREE;\n\
           \        ORTO_SLOTS[i].waiter = NULL;\n\
           \    }\n\
           \    ORTO_SLOTS[ORTO_SLOT_COUNT - 1].next_free = -1;\n\
           \    ORTO_SLOT_FREE_HEAD = 0;\n\
           }"
          frame_union slots
      in
      let frames = List.map emit_async_frame_struct async_funcs in
      (* Forward decls for every <name>_step so sync callers (and the
         sync wrappers themselves, which are emitted before
         async_defs) can refer to them. *)
      let step_fwds =
        List.map (fun (f : Check.T.func) ->
          Printf.sprintf "static int %s_step(void *frp);" f.name)
          async_funcs
      in
      let steps  = List.map (emit_async_step ctor_map) async_funcs in
      let wrappers =
        List.map (fun (f : Check.T.func) ->
          if f.name = "main" then emit_async_main_wrapper f
          else emit_async_sync_wrapper f) async_funcs
      in
      (rt, frames @ step_fwds, steps @ wrappers)
    end
  in
  let static_bytes = emit_static_bytes_array () in
  let header = Printf.sprintf
    "/* generated by orto */\n\
     #include <stdlib.h>\n\
     #include <stddef.h>\n\
     #include <stdint.h>\n\
     #include <string.h>\n\
     \n\
     /* Concurrency mode, set by orto's --cores N flag (default 1).\n\
      * cores=1 → single-thread runtime, no pthread dependency.\n\
      * cores>1 → shared-nothing per-core: every runtime global\n\
      * lives in thread-local storage; main forks N pthread\n\
      * workers, each running its own copy of the program. */\n\
     #define ORTO_CORES %d\n\
     /* Size of the io_uring submission/completion ring (in SQEs).\n\
      * Controlled by --ring-entries N. Larger = more concurrent\n\
      * SQEs may live in-kernel; smaller = less per-thread memory.\n\
      * Must be a power of two; clamped by the kernel to its max. */\n\
     #define ORTO_RING_ENTRIES %d\n\
     #if ORTO_CORES > 1\n\
     #  include <pthread.h>\n\
     #  define ORTO_TLS __thread\n\
     #else\n\
     #  define ORTO_TLS\n\
     #endif\n\
     \n\
     /* Region runtime: a global slab of region slots. Each slot is\n\
      * reused after its region is dropped (gen bumps so old handles\n\
      * see a mismatch and either abort or take the dangling branch).\n\
      * No allocation per region beyond the user-requested buffer.\n\
      * Slot 0 is reserved for the static string-literal pool. */\n\
     #define ORTO_REGION_SLOTS 4096\n\
     /* The handle is a (slot, expected_gen) pair. expected_gen is\n\
      * 64-bit so it can't wrap in any realistic uptime — even at\n\
      * 1G reuses/s per slot, wrapping takes ~300 years. */\n\
     struct Region_slot {\n\
     \    long long gen;\n\
     \    char* buffer;\n\
     \    size_t buffer_size;\n\
     \    size_t used;\n\
     \    int next_free;   /* -1 if in use, else next free slot id */\n\
     \    int is_stack;    /* 1 if buffer is stack memory (do not free) */\n\
     };\n\
     typedef struct { int slot; long long expected_gen; } Region;\n\
     ORTO_TLS struct Region_slot ORTO_REGIONS[ORTO_REGION_SLOTS];\n\
     ORTO_TLS int ORTO_REGION_FREE_HEAD = -1;\n\
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
    cores ring_entries static_bytes
  in
  let async_prelude =
    if async_funcs = [] then []
    else ["typedef int (*OrtoStepFn)(void *frame_ptr);"]
  in
  String.concat "\n\n"
    ([header]
     @ adt_forwards
     @ rec_forwards
     @ array_forwards
     @ task_forwards
     @ fn_typedefs
     @ struct_defs
     @ tuple_forwards
     @ async_prelude
     @ async_decls
     @ (if async_runtime = "" then [] else [async_runtime])
     @ extern_decls
     @ decls
     @ async_decls_sync
     @ array_drop_forwards
     @ task_drop_forwards
     @ stream_drop_forwards
     @ tuple_drop_forwards
     @ array_drop_defs
     @ tuple_drop_defs
     @ task_drop_defs
     @ stream_drop_defs
     @ defs
     @ async_defs)

(* QBE backend — alternative to the C emitter (emit.ml).
   Front-end (parse→resolve→lift→check→mono) shared; lowers the typed,
   monomorphized program to QBE IL (.ssa).
     M1: functions, literals, ret.
     M2: arithmetic, comparisons, let (stack slots), if/while, jumps.
     M3a: calls, recursion, externs; numeric conversions.
     M4: structs/enums/match — aggregates as pointers + manual layout
         (load/store/blit), aggregate return via a hidden sret pointer.
   Unlowered nodes raise failwith so tests only hit implemented constructs. *)

open Check.T
module A = Ast

(* ===== program-wide layout (filled in `emit`) ===== *)
let records : (string, A.record_decl) Hashtbl.t = Hashtbl.create 64
let enums   : (string, A.type_decl) Hashtbl.t = Hashtbl.create 64

let is_record n = Hashtbl.mem records n
let is_enum   n = Hashtbl.mem enums n

let rec is_agg (t : A.ty) : bool =
  match t with
  | A.TyApp (n, _) when is_record n || is_enum n -> true
  | A.TyApp ("Handle", _) | A.TyApp ("Region", []) -> true
  | A.TyTuple (_ :: _) -> true
  | A.TyFun _ -> true        (* function value = fat pointer {env, code} *)
  | _ -> false

and scalar_size (t : A.ty) : int =
  match t with
  | A.TyBool | A.TyApp (("byte"|"u8"|"i8"), []) -> 1
  | A.TyApp (("u16"|"i16"), []) -> 2
  | A.TyApp (("u32"|"i32"|"f32"), []) -> 4
  | A.TyApp (("u128"|"i128"|"f128"), []) -> 16
  | _ -> 8

and align_of (t : A.ty) : int =
  if not (is_agg t) then scalar_size t
  else match t with
    | A.TyApp ("Region", []) | A.TyApp ("Handle", _) -> 8
    | A.TyApp (n, _) when is_record n ->
        List.fold_left (fun m (_, ft) -> max m (align_of ft)) 1
          (Hashtbl.find records n).A.rec_fields
    | A.TyApp (n, _) when is_enum n -> 8
    | A.TyTuple ts -> List.fold_left (fun m ft -> max m (align_of ft)) 1 ts
    | _ -> 8

and round_up x a = if a <= 0 then x else (x + a - 1) / a * a

and size_of (t : A.ty) : int =
  match t with
  | _ when not (is_agg t) -> scalar_size t
  | A.TyFun _ -> 16          (* {env: l; code: l} *)
  | A.TyApp ("Region", []) -> 16
  | A.TyApp ("Handle", _) -> 32
  | A.TyApp (n, _) when is_record n ->
      let off = List.fold_left (fun off (_, ft) ->
        round_up off (align_of ft) + size_of ft) 0
        (Hashtbl.find records n).A.rec_fields in
      round_up off (align_of t)
  | A.TyApp (n, _) when is_enum n ->
      let td = Hashtbl.find enums n in
      let payload =
        List.fold_left (fun m v ->
          let off = List.fold_left (fun off ft ->
            round_up off (align_of ft) + size_of ft) 0 v.A.arg_tys in
          max m off) 0 td.A.variants
      in
      round_up (8 + payload) 8
  | A.TyTuple ts ->
      let off = List.fold_left (fun off ft ->
        round_up off (align_of ft) + size_of ft) 0 ts in
      round_up off (align_of t)
  | _ -> 8

(* record fields: (name, field_ty, offset), naturally aligned *)
let record_fields (n : string) : (string * A.ty * int) list =
  let rd = Hashtbl.find records n in
  let (_, rev) =
    List.fold_left (fun (off, acc) (fn, ft) ->
      let o = round_up off (align_of ft) in
      (o + size_of ft, (fn, ft, o) :: acc)) (0, []) rd.A.rec_fields
  in
  List.rev rev

(* enum constructor: tag index + payload arg layout [(arg_ty, offset)],
   payload after the 8-byte tag, naturally aligned. *)
let ctor_layout (enum_name : string) (ctor : string) : int * (A.ty * int) list =
  let td = Hashtbl.find enums enum_name in
  let rec find i = function
    | [] -> failwith (Printf.sprintf "qbe: ctor %s not in enum %s" ctor enum_name)
    | v :: _ when v.A.ctor_name = ctor -> (i, v)
    | _ :: rest -> find (i + 1) rest
  in
  let (idx, v) = find 0 td.A.variants in
  let (_, rev) =
    List.fold_left (fun (off, acc) ft ->
      let o = round_up off (align_of ft) in
      (o + size_of ft, (ft, o) :: acc)) (8, []) v.A.arg_tys
  in
  (idx, List.rev rev)

let enum_name_of (t : A.ty) =
  match t with A.TyApp (n, _) when is_enum n -> n | _ -> failwith "qbe: not an enum type"
let record_name_of (t : A.ty) =
  match t with A.TyApp (n, _) when is_record n -> n | _ -> failwith "qbe: not a record type"

(* ===== width class of a scalar orto type (mirrors the QBE "w/l/s/d") ===== *)
(* "w" = 32-bit int SSA (i32), "l" = 64-bit int (i64), "s" = f32, "d" = f64. *)
let qty (t : A.ty) : string =
  match t with
  | A.TyInt -> "l"
  | A.TyBool -> "w"
  | A.TyApp (("byte"|"u8"|"i8"|"u16"|"i16"|"u32"|"i32"), []) -> "w"
  | A.TyApp (("u64"|"i64"|"u128"|"i128"), []) -> "l"
  | A.TyApp ("f32", []) -> "s"
  | A.TyApp (("f64"|"float"), []) -> "d"
  | A.TyPtr _ -> "l"
  | _ -> "l"   (* aggregates are passed as pointers (l) *)

(* CLIF type name for a width class *)
let ctype = function "w" -> "i32" | "l" -> "i64" | "s" -> "f32" | "d" -> "f64" | x -> x

(* ===== per-function emit state ===== *)
(* CLIF demands proper SSA blocks; we mirror the QBE backend's stack-slot
   structure, so each "label" is a block and every block ends in a terminator.
   [done_blocks] accumulates finished blocks; [cur] is the open block's body. *)
let done_blocks = Buffer.create 2048
let cur = Buffer.create 1024
let entry_buf = Buffer.create 256     (* instrs hoisted into the entry block (stack_addr etc.) *)
let pre_buf = Buffer.create 256       (* preamble: explicit_slot / sig / fn decls *)
let tmp = ref (-1)
let blk = ref (-1)
let cur_name = ref "block0"
let cur_name_block0 = ref "block0"
let cur_term = ref false
let first_block = ref true
let fresh () = incr tmp; Printf.sprintf "v%d" !tmp
(* a fresh block name; returns the bare label like "block3" *)
let flabel _p = incr blk; Printf.sprintf "block%d" !blk
let ins fmt = Printf.ksprintf (fun s -> Buffer.add_string cur ("    " ^ s ^ "\n")) fmt
(* hoisted-to-entry instructions (run once, in the entry block) *)
let ains fmt = Printf.ksprintf (fun s -> Buffer.add_string entry_buf ("    " ^ s ^ "\n")) fmt

(* flush the current block into [done_blocks]: header, [entry_buf] if it's the
   very first block, then the body. *)
let flush_cur () =
  Buffer.add_string done_blocks (!cur_name ^ ":\n");
  Buffer.add_buffer done_blocks cur;
  Buffer.clear cur

(* open a new block named [l]. If the current block has no terminator, fall
   through to [l] explicitly (CLIF requires every block to terminate). *)
let label l =
  if not !cur_term then Buffer.add_string cur (Printf.sprintf "    jump %s\n" l);
  flush_cur ();
  cur_name := l; cur_term := false

(* terminator helpers *)
let mark_term () = cur_term := true
let jmp target = ins "jump %s" target; mark_term ()
let brif c t f = ins "brif %s, %s, %s" c t f; mark_term ()
(* a terminator that ends a straight-line region (ret/abort): emit it then open
   a fresh dead block so any trailing code has a well-formed home. *)
let term fmt =
  Printf.ksprintf (fun s ->
    Buffer.add_string cur ("    " ^ s ^ "\n");
    mark_term ();
    label (flabel "dead")) fmt

(* ----- preamble decl management (slots / call sigs / funcrefs) ----- *)
let slot_n = ref 0
let sig_n = ref 0
let fn_n = ref 0
(* sig signature string -> sigN ; funcref "name@sigN" -> fnN *)
let sig_tbl : (string, string) Hashtbl.t = Hashtbl.create 16
let fn_tbl  : (string, string) Hashtbl.t = Hashtbl.create 16

(* declare an explicit stack slot of [bytes] and return a pointer value to it,
   emitting the stack_addr in the entry block (once). *)
let alloc (bytes : int) : string =
  let b = if bytes < 1 then 1 else bytes in
  let s = Printf.sprintf "ss%d" !slot_n in incr slot_n;
  Buffer.add_string pre_buf (Printf.sprintf "    %s = explicit_slot %d\n" s b);
  let v = fresh () in
  Buffer.add_string entry_buf (Printf.sprintf "    %s = stack_addr.i64 %s\n" v s);
  v

(* materialize a constant of width class q *)
let iconst (q : string) (lit : string) : string =
  let v = fresh () in
  (match q with
   | "s" -> ins "%s = f32const %s" v lit
   | "d" -> ins "%s = f64const %s" v lit
   | _   -> ins "%s = iconst.%s %s" v (ctype q) lit);
  v

(* register a call signature (arg width classes, return width class option),
   return its sigN name. *)
let get_sig (argqs : string list) (retq : string option) : string =
  let argstr = String.concat ", " (List.map ctype argqs) in
  let key = match retq with
    | Some r -> Printf.sprintf "(%s) -> %s system_v" argstr (ctype r)
    | None   -> Printf.sprintf "(%s) system_v" argstr in
  match Hashtbl.find_opt sig_tbl key with
  | Some s -> s
  | None ->
      let s = Printf.sprintf "sig%d" !sig_n in incr sig_n;
      Hashtbl.replace sig_tbl key s;
      Buffer.add_string pre_buf (Printf.sprintf "    %s = %s\n" s key);
      s

(* register a funcref to %name with a given sig, return its fnN name. *)
let get_fn (name : string) (sg : string) : string =
  let key = name ^ "@" ^ sg in
  match Hashtbl.find_opt fn_tbl key with
  | Some f -> f
  | None ->
      let f = Printf.sprintf "fn%d" !fn_n in incr fn_n;
      Hashtbl.replace fn_tbl key f;
      Buffer.add_string pre_buf (Printf.sprintf "    %s = %%%s %s\n" f name sg);
      f

(* emit a direct call to a runtime/orto function; returns result value or "" *)
let do_call (name : string) (args : (string * string) list) (retq : string option) : string =
  let sg = get_sig (List.map fst args) retq in
  let fn = get_fn name sg in
  let argstr = String.concat ", " (List.map snd args) in
  match retq with
  | None -> ins "call %s(%s)" fn argstr; ""
  | Some _ -> let v = fresh () in ins "%s = call %s(%s)" v fn argstr; v

(* memcpy(dst, src, n) via libc *)
let blit (src : string) (dst : string) (sz : int) : unit =
  let n = iconst "l" (string_of_int sz) in
  ignore (do_call "memcpy" [("i64", dst); ("i64", src); ("i64", n)] (Some "l"))

(* abort the program (matches QBE's $abort exit code): call abort, then a trap
   to terminate the block (abort is noreturn but CLIF still needs a terminator). *)
let do_abort () : unit =
  let fn = get_fn "abort" (get_sig [] None) in
  ins "call %s()" fn;
  term "trap heap_oob"

(* width-correct store of [v] (a value of width q) of orto type [t] to ptr [p] *)
let store_ty (t : A.ty) (v : string) (p : string) : unit =
  match scalar_size t, qty t with
  | 1, _ -> ins "istore8 %s, %s" v p
  | 2, _ -> ins "istore16 %s, %s" v p
  | 4, "s" -> ins "store %s, %s" v p
  | 4, _ -> ins "store %s, %s" v p          (* v is i32 *)
  | _, "d" -> ins "store %s, %s" v p
  | _, "s" -> ins "store %s, %s" v p
  | _ -> ins "store %s, %s" v p              (* i64 *)

(* width-correct load of orto type [t] from ptr [p]; returns a value of width q *)
let load_ty (t : A.ty) (p : string) : string =
  let v = fresh () in
  (match scalar_size t, qty t with
   | 1, _ -> ins "%s = uload8.i32 %s" v p
   | 2, _ -> ins "%s = uload16.i32 %s" v p
   | 4, "s" -> ins "%s = load.f32 %s" v p
   | 4, _ -> ins "%s = load.i32 %s" v p
   | _, "d" -> ins "%s = load.f64 %s" v p
   | _, "s" -> ins "%s = load.f32 %s" v p
   | _ -> ins "%s = load.i64 %s" v p);
  v

(* store/load a pointer-sized (i64) value, used for if/match join slots *)
let store_l (v : string) (p : string) : unit = ins "store %s, %s" v p
let load_l (p : string) : string =
  let v = fresh () in ins "%s = load.i64 %s" v p; v
(* store/load a value of arbitrary width class q (join slots for scalars) *)
let store_q (q : string) (v : string) (p : string) : unit =
  match q with
  | "w" -> ins "store %s, %s" v p           (* store the full i32; slot is 8 bytes *)
  | _ -> ins "store %s, %s" v p
let load_q (q : string) (p : string) : string =
  let v = fresh () in
  (match q with
   | "w" -> ins "%s = load.i32 %s" v p
   | "s" -> ins "%s = load.f32 %s" v p
   | "d" -> ins "%s = load.f64 %s" v p
   | _   -> ins "%s = load.i64 %s" v p);
  v

(* a comparison value (icmp/fcmp produce i8); widen to i32 for storing as bool *)
let cmp_to_w (cmpv : string) : string =
  let v = fresh () in ins "%s = uextend.i32 %s" v cmpv; v

type local = Scal of string * A.ty | Agg of string * int
let locals : (string, local) Hashtbl.t = Hashtbl.create 16
let loops : (string * string) list ref = ref []
let cur_sret : string option ref = ref None     (* Some ptr if fn returns an aggregate *)
let cur_ret_size : int ref = ref 0
let cur_ret_q : string ref = ref "l"            (* width class of the function's return *)

(* ===== type of an expression node ===== *)
let ty_of (e : expr) : A.ty =
  match e with
  | TEInt _ -> A.TyInt
  | TEBool _ -> A.TyBool
  | TEVar (_, t) -> t
  | TEBinop (_, _, _, t) -> t
  | TEUnop (_, _, t) -> t
  | TEIf (_, _, _, t) -> t
  | TELet (_, _, _, _, t, _) -> t
  | TECast (n, _) -> A.TyApp (n, [])
  | TEToInt _ | TEToIntFromFloat _ -> A.TyInt
  | TEToByte _ -> A.TyApp ("byte", [])
  | TEToFloat _ -> A.TyApp ("float", [])
  | TECall (_, _, t) -> t
  | TEFnRef (_, _, t) -> t
  | TEStringLit _ -> A.TyApp ("Handle", [A.TyApp ("byte", [])])
  | TEField (_, _, t) -> t
  | TERecord (n, _, _, _) -> A.TyApp (n, [])
  | TECtor (_, _, _, t) -> t
  | TEMatch (_, _, _, t) -> t
  | TEReturn _ -> A.TyInt
  | TEFloat _ -> A.TyApp ("float", [])
  | TEIndex (_, _, t) -> t
  | TEDeref (_, t) -> t
  | TEHandle (_, _, _, t) -> t
  | TEHandleLit (_, _, t) -> t
  | TERegion (_, t) | TEStackRegion (_, t) | TEAlignedRegion (_, _, t) -> t
  | TESlice (_, _, _, t) -> t
  | TEHandleData (_, t) -> t
  | TEPtrCast (_, t) -> t
  | TECAlloc (_, _, t) -> t
  | TENullPtr t -> t
  | TETryAt (_, _, t) -> t
  | TETuple (_, t) -> t
  | TETupleIdx (_, _, t) -> t
  | _ -> A.TyInt

let handle_elem (t : A.ty) : A.ty =
  match t with A.TyApp ("Handle", [e]) -> e | _ -> A.TyApp ("byte", [])

(* the CLIF width class that [lower e] actually produces. Most nodes yield
   qty (ty_of e); the side-effecting / control nodes yield an i32 "0". *)
let rec lowered_q (e : expr) : string =
  match e with
  | TEAssign _ | TEAssignIdx _ | TEAssignField _ | TEReturn _ | TEBreak | TEContinue
  | TEReset _ | TEDrop _ | TEPrint _ | TECFree _ | TEWhile _ -> "w"
  (* a let/let-tuple's value is that of its body (its tail expression) *)
  | TELet (_, _, _, body, _, _) | TELetTuple (_, _, _, body, _, _) -> lowered_q body
  | _ -> qty (ty_of e)

(* ===== LICM of loop-invariant handle resolution =====
   The gen+bounds check (orto_rt_at) re-derives a handle's buffer/len every
   access. When a handle var is loop-invariant and the loop body can't
   reset/drop its region (no calls/reset/drop/async), we resolve it ONCE in a
   guarded preheader (base = data(h), len = len(h)) and inside the loop emit
   only a cheap inline bounds check + address. Verified ~11% on a synthetic
   kernel; unlike inline/CSE it REDUCES per-iteration work. *)

(* total immediate-subexpression enumeration (no wildcard ⇒ compiler enforces
   completeness, so a new node can't silently break the soundness analysis). *)
let subexprs (e : expr) : expr list =
  match e with
  | TEInt _ | TEBool _ | TEVar _ | TEStringLit _ | TEFnRef _ | TEFloat _
  | TENullPtr _ | TEBreak | TEContinue -> []
  | TECall (c, args, _) -> c :: args
  | TEBinop (_, a, b, _) -> [a; b]
  | TEUnop (_, a, _) -> [a]
  | TECtor (_, _, args, _) -> args
  | TERecord (_, _, fs, _) -> List.map snd fs
  | TEField (a, _, _) -> [a]
  | TEIf (a, b, c, _) -> [a; b; c]
  | TELet (_, _, v, b, _, _) -> [v; b]
  | TEMatch (s, _, arms, _) ->
      s :: List.concat_map (fun (_, g, b) ->
        (match g with Some x -> [x] | None -> []) @ [b]) arms
  | TEHandle (a, b, c, _) -> [a; b; c]
  | TEHandleLit (r, els, _) -> r :: els
  | TERegion (a, _) | TEStackRegion (a, _) -> [a]
  | TEAlignedRegion (a, b, _) -> [a; b]
  | TEIndex (a, b, _) -> [a; b]
  | TEAssignIdx (a, b, c, _) -> [a; b; c]
  | TELen (a, _) -> [a]
  | TESlice (a, b, c, _) -> [a; b; c]
  | TEToInt a | TEToByte a | TEToFloat a | TEToIntFromFloat a -> [a]
  | TECast (_, a) -> [a]
  | TECAlloc (_, a, _) -> [a]
  | TECFree a -> [a]
  | TEIsNull a -> [a]
  | TEHandleData (a, _) -> [a]
  | TEPtrCast (a, _) -> [a]
  | TETryAt (a, b, _) -> [a; b]
  | TEDrop (a, _) -> [a]
  | TEReset a -> [a]
  | TEDeref (a, _) -> [a]
  | TEAssign (_, v, _) -> [v]
  | TEAssignField (p, _, v) -> [p; v]
  | TEWhile (c, b) -> [c; b]
  | TEReturn (a, _) -> [a]
  | TEAwait (a, _, _) -> [a]
  | TESpawn (a, _) -> [a]
  | TEForStream (_, _, s, b) -> [s; b]
  | TETuple (els, _) -> els
  | TETupleIdx (a, _, _) -> [a]
  | TELetTuple (_, _, v, b, _, _) -> [v; b]
  | TEAwaitAll (els, _, _) -> els
  | TEMakeClosure (_, _, _, r, _) -> [r]
  | TEPrint (_, els, _) -> els

let rec walk (f : expr -> unit) (e : expr) : unit =
  f e; List.iter (walk f) (subexprs e)

(* region_safe[name] = the function provably never (transitively) resets or
   drops a region and makes no indirect (closure) call. Computed once per
   program in `emit`. Externs (absent here) are treated as safe — foreign C
   can't touch our region runtime's gen/used/free state. *)
let region_safe : (string, bool) Hashtbl.t = Hashtbl.create 64

(* a node that directly resets/drops a region or hides an unknown callee *)
let directly_region_affecting (body : expr) : bool =
  let bad = ref false in
  walk (fun e -> match e with
    | TEReset _ | TEDrop _ | TESpawn _ | TEAwait _ | TEForStream _ | TEAwaitAll _ -> bad := true
    | TECall (TEFnRef _, _, _) -> ()          (* direct call: handled by propagation *)
    | TECall (_, _, _) -> bad := true         (* indirect call: unknown target *)
    | _ -> ()) body;
  !bad

let direct_callees (body : expr) : string list =
  let acc = ref [] in
  walk (fun e -> match e with
    | TECall (TEFnRef (n, _, _), _, _) -> if not (List.mem n !acc) then acc := n :: !acc
    | _ -> ()) body;
  !acc

(* a loop body is safe to hoist a handle resolution out of iff nothing in it
   could reset/drop the region (directly or via a called function). *)
let body_hoist_safe (body : expr) : bool =
  let bad = ref false in
  walk (fun e -> match e with
    | TEReset _ | TEDrop _ | TESpawn _ | TEAwait _ | TEForStream _ | TEAwaitAll _ -> bad := true
    | TECall (TEFnRef (n, _, _), _, _) ->
        (match Hashtbl.find_opt region_safe n with Some false -> bad := true | _ -> ())
    | TECall (_, _, _) -> bad := true
    | _ -> ()) body;
  not !bad

(* fixpoint over the call graph: a function is region-unsafe if it directly
   resets/drops/indirect-calls, or calls another region-unsafe function. *)
let compute_region_safe (funcs : func list) : unit =
  Hashtbl.clear region_safe;
  let unsafe : (string, unit) Hashtbl.t = Hashtbl.create 64 in
  List.iter (fun (f : func) -> if directly_region_affecting f.body then Hashtbl.replace unsafe f.name ()) funcs;
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter (fun (f : func) ->
      if not (Hashtbl.mem unsafe f.name)
      && List.exists (fun c -> Hashtbl.mem unsafe c) (direct_callees f.body)
      then (Hashtbl.replace unsafe f.name (); changed := true)) funcs
  done;
  List.iter (fun (f : func) -> Hashtbl.replace region_safe f.name (not (Hashtbl.mem unsafe f.name))) funcs

let reassigned_in (body : expr) : (string, unit) Hashtbl.t =
  let s = Hashtbl.create 8 in
  walk (fun e -> match e with
    | TEAssign (n, _, _) -> Hashtbl.replace s n ()
    | TELet (n, _, _, _, _, _) -> Hashtbl.replace s n ()
    | TELetTuple (ns, _, _, _, _, _) -> List.iter (fun n -> Hashtbl.replace s n ()) ns
    | TEForStream (n, _, _, _) -> Hashtbl.replace s n ()
    | _ -> ()) body; s

(* handle-typed vars that are indexed somewhere in the body, with their type *)
let indexed_handles (body : expr) : (string * A.ty) list =
  let acc = ref [] in
  walk (fun e -> match e with
    | TEIndex (TEVar (v, (A.TyApp ("Handle", [_]) as t)), _, _) ->
        if not (List.mem_assoc v !acc) then acc := (v, t) :: !acc
    | _ -> ()) body;
  List.rev !acc

(* var -> (base_ptr_temp, len_temp, elemsize) hoisted for the enclosing loop *)
let hoisted : (string, string * string * int) Hashtbl.t = Hashtbl.create 8

(* closure environment layout (captures laid out like a struct) *)
let capture_layout (caps : (string * A.ty) list) : (string * A.ty * int) list =
  let (_, rev) = List.fold_left (fun (off, acc) (n, t) ->
    let o = round_up off (align_of t) in (o + size_of t, (n, t, o) :: acc)) (0, []) caps in
  List.rev rev
let capture_size caps =
  List.fold_left (fun m (_, t, o) -> max m (o + size_of t)) 8 (capture_layout caps)

(* plain top-level fns used as values need an env-adapting wrapper *)
let wrappers : (string, A.ty) Hashtbl.t = Hashtbl.create 16

(* string literals -> data labels, deduped; emitted at top of the module *)
let strings : (string, string) Hashtbl.t = Hashtbl.create 32
let intern_string (s : string) : string =
  match Hashtbl.find_opt strings s with
  | Some lab -> lab
  | None -> let lab = Printf.sprintf "$str.%d" (Hashtbl.length strings) in
            Hashtbl.replace strings s lab; lab

(* ===== numeric conversion (width classes w=i32 l=i64 s=f32 d=f64) ===== *)
let convert (sq : string) (dq : string) (v : string) : string =
  if sq = dq then v
  else begin
    let r = fresh () in
    (match sq, dq with
     | "w", "l"       -> ins "%s = sextend.i64 %s" r v
     | "l", "w"       -> ins "%s = ireduce.i32 %s" r v
     | ("w"|"l"), "d" -> ins "%s = fcvt_from_sint.f64 %s" r v
     | ("w"|"l"), "s" -> ins "%s = fcvt_from_sint.f32 %s" r v
     | "d", ("w"|"l") -> ins "%s = fcvt_to_sint.%s %s" r (ctype dq) v
     | "s", ("w"|"l") -> ins "%s = fcvt_to_sint.%s %s" r (ctype dq) v
     | "s", "d"       -> ins "%s = fpromote.f64 %s" r v
     | "d", "s"       -> ins "%s = fdemote.f32 %s" r v
     | _              -> ins "%s = ireduce.%s %s" r (ctype dq) v);
    r
  end

(* ===== lower an expression to a CLIF value ===== *)
let rec lower (e : expr) : string =
  match e with
  | TEInt n  -> iconst "l" (Int64.to_string n)
  | TEBool b -> iconst "w" (if b then "1" else "0")
  | TEFloat f ->
      (* CLIF f64const wants a hex float; %h gives e.g. 0x1.4p+1 but cranelift
         rejects the '+' in the exponent (strip it) and requires a fractional
         '.' before the 'p' (insert ".0" if missing, e.g. 0x1p1 -> 0x1.0p1). *)
      let h = Printf.sprintf "%h" f in
      let h = String.concat "" (List.map (fun c -> if c = '+' then "" else String.make 1 c)
                                  (List.init (String.length h) (String.get h))) in
      let h =
        if String.contains h '.' then h
        else match String.index_opt h 'p' with
          | Some pi -> String.sub h 0 pi ^ ".0" ^ String.sub h pi (String.length h - pi)
          | None -> h ^ ".0"
      in
      iconst "d" h

  | TEVar (name, _) ->
      (match Hashtbl.find locals name with
       | Agg (ptr, _) -> ptr                       (* value of an aggregate = its address *)
       | Scal (slot, ty) -> load_ty ty slot)

  | TELet (name, vty, value, body, _, _) ->
      if is_agg vty then begin
        let v = lower value in
        let sz = size_of vty in
        let p = alloc sz in
        blit v p sz;
        Hashtbl.replace locals name (Agg (p, sz))
      end else begin
        let v = lower value in
        let slot = alloc 8 in
        store_ty vty v slot;
        Hashtbl.replace locals name (Scal (slot, vty))
      end;
      lower body

  | TEAssign (name, value, _) ->
      (match Hashtbl.find locals name with
       | Scal (slot, ty) -> let v = lower value in store_ty ty v slot
       | Agg (ptr, sz)  -> let v = lower value in blit v ptr sz);
      iconst "w" "0"

  (* pointer arithmetic: p ± n advances by n elements; p - q is element diff *)
  | TEBinop ((A.OpAdd | A.OpSub) as op, a, b, _)
    when (match ty_of a with A.TyPtr _ -> true | _ -> false)
      && (match ty_of b with A.TyPtr _ -> false | _ -> true) ->
      let es = (match ty_of a with A.TyPtr e -> size_of e | _ -> 1) in
      let pv = lower a in let nv = convert (qty (ty_of b)) "l" (lower b) in
      let esv = iconst "l" (string_of_int es) in
      let scaled = fresh () in ins "%s = imul %s, %s" scaled nv esv;
      let r = fresh () in
      ins "%s = %s %s, %s" r (if op = A.OpAdd then "iadd" else "isub") pv scaled; r
  | TEBinop (A.OpSub, a, b, _)
    when (match ty_of a, ty_of b with A.TyPtr _, A.TyPtr _ -> true | _ -> false) ->
      let es = (match ty_of a with A.TyPtr e -> size_of e | _ -> 1) in
      let pa = lower a in let pb = lower b in
      let d = fresh () in ins "%s = isub %s, %s" d pa pb;
      let esv = iconst "l" (string_of_int es) in
      let r = fresh () in ins "%s = sdiv %s, %s" r d esv; r

  (* short-circuit && and || *)
  | TEBinop (A.OpAnd, a, b, _) ->
      let rslot = alloc 8 in
      let va = lower a in
      let lb = flabel "andb" and lf = flabel "andf" and le = flabel "ande" in
      brif va lb lf;
      label lb; let vb = lower b in store_q "w" vb rslot; jmp le;
      label lf; store_q "w" (iconst "w" "0") rslot; jmp le;
      label le; load_q "w" rslot
  | TEBinop (A.OpOr, a, b, _) ->
      let rslot = alloc 8 in
      let va = lower a in
      let lt = flabel "ort" and lb = flabel "orb" and le = flabel "ore" in
      brif va lt lb;
      label lt; store_q "w" (iconst "w" "1") rslot; jmp le;
      label lb; let vb = lower b in store_q "w" vb rslot; jmp le;
      label le; load_q "w" rslot

  | TEBinop (op, a, b, t) ->
      let va = lower a in
      let vb = lower b in
      let aq = qty (ty_of a) in
      let rq = qty t in
      let isflt = aq = "d" || aq = "s" in
      let arith name = let r = fresh () in ins "%s = %s %s, %s" r name va vb; ignore rq; r in
      let cmp code = let c = fresh () in
        ins "%s = %s %s %s, %s" c (if isflt then "fcmp" else "icmp") code va vb;
        cmp_to_w c in
      (match op with
       | A.OpAdd  -> arith (if isflt then "fadd" else "iadd")
       | A.OpSub  -> arith (if isflt then "fsub" else "isub")
       | A.OpMul  -> arith (if isflt then "fmul" else "imul")
       | A.OpDiv  -> arith (if isflt then "fdiv" else "sdiv")
       | A.OpMod  -> arith "srem"
       | A.OpBAnd | A.OpAnd -> arith "band"
       | A.OpBOr  | A.OpOr  -> arith "bor"
       | A.OpBXor -> arith "bxor"
       | A.OpShl  -> arith "ishl"
       | A.OpShr  -> arith "sshr"
       | A.OpEq   -> cmp "eq"
       | A.OpNeq  -> cmp "ne"
       | A.OpLt   -> cmp (if isflt then "lt" else "slt")
       | A.OpLe   -> cmp (if isflt then "le" else "sle")
       | A.OpGt   -> cmp (if isflt then "gt" else "sgt")
       | A.OpGe   -> cmp (if isflt then "ge" else "sge"))

  | TEUnop (op, a, t) ->
      let va = lower a in
      let r = fresh () in
      let q = qty t in
      (match op with
       | A.OpNeg  -> if q = "d" || q = "s" then ins "%s = fneg %s" r va
                     else (let z = iconst q "0" in ins "%s = isub %s, %s" r z va)
       | A.OpBNot -> let m = iconst q "-1" in ins "%s = bxor %s, %s" r va m
       | A.OpNot  -> let z = iconst (qty (ty_of a)) "0" in
                     let c = fresh () in ins "%s = icmp eq %s, %s" c va z;
                     ins "%s = uextend.i32 %s" r c);
      r

  | TEIf (c, th, el, t) ->
      let agg = is_agg t in
      let q = qty t in
      let rslot = alloc 8 in
      let lt = flabel "then" and le = flabel "else" and lj = flabel "join" in
      let vc = lower c in
      brif vc lt le;
      label lt;
      let vt = lower th in (if agg then store_l vt rslot else store_q q vt rslot); jmp lj;
      label le;
      let ve = lower el in (if agg then store_l ve rslot else store_q q ve rslot); jmp lj;
      label lj;
      if agg then load_l rslot else load_q q rslot

  | TEWhile (c, body) ->
      let hoist =
        if body_hoist_safe body then begin
          let reassigned = reassigned_in body in
          List.filter (fun (v, _) ->
            not (Hashtbl.mem reassigned v) && not (Hashtbl.mem hoisted v))
            (indexed_handles body)
        end else []
      in
      if hoist = [] then begin
        let lc = flabel "loop" and lb = flabel "body" and le = flabel "end" in
        jmp lc;
        label lc; let vc = lower c in brif vc lb le;
        label lb;
        loops := (lc, le) :: !loops;
        let _ = lower body in
        loops := List.tl !loops;
        jmp lc;
        label le; iconst "w" "0"
      end else begin
        let lpre = flabel "pre" and lc = flabel "loop"
        and lb = flabel "body" and le = flabel "end" in
        let vc0 = lower c in brif vc0 lpre le;
        label lpre;
        let added = List.map (fun (v, t) ->
          let es = size_of (handle_elem t) in
          let hv = lower (TEVar (v, t)) in
          let base = do_call "orto_rt_data" [("i64", hv)] (Some "l") in
          let len = do_call "orto_rt_len" [("i64", hv)] (Some "l") in
          Hashtbl.replace hoisted v (base, len, es); v) hoist in
        jmp lb;
        label lb;
        loops := (lc, le) :: !loops;
        let _ = lower body in
        loops := List.tl !loops;
        jmp lc;
        label lc; let vc = lower c in brif vc lb le;
        label le;
        List.iter (Hashtbl.remove hoisted) added;
        iconst "w" "0"
      end

  | TEBreak    -> let (_, b) = List.hd !loops in jmp b; label (flabel "dead"); iconst "w" "0"
  | TEContinue -> let (c, _) = List.hd !loops in jmp c; label (flabel "dead"); iconst "w" "0"

  | TEReturn (v, _) ->
      (match !cur_sret with
       | Some s -> let p = lower v in blit p s !cur_ret_size; term "return"
       | None   -> let vv = convert (qty (ty_of v)) !cur_ret_q (lower v) in
                   term "return %s" vv);
      iconst "w" "0"

  | TEToInt e          -> convert (qty (ty_of e)) "l" (lower e)
  | TEToIntFromFloat e -> convert (qty (ty_of e)) "l" (lower e)
  | TEToFloat e        -> convert (qty (ty_of e)) "d" (lower e)
  | TEToByte e ->
      let v = convert (qty (ty_of e)) "w" (lower e) in
      let m = iconst "w" "255" in
      let r = fresh () in ins "%s = band %s, %s" r v m; r
  | TECast (target, e) ->
      convert (qty (ty_of e)) (qty (A.TyApp (target, []))) (lower e)

  (* ----- aggregates ----- *)
  | TERecord (name, _, fields, _) ->
      let sz = size_of (A.TyApp (name, [])) in
      let p = alloc sz in
      let layout = record_fields name in
      List.iter (fun (fn, ft, off) ->
        let value = List.assoc fn fields in
        let v = lower value in
        let fp = field_ptr p off in
        if is_agg ft then blit v fp (size_of ft)
        else store_ty ft v fp) layout;
      p

  | TEField (e, fname, fty) ->
      let base = lower e in
      let rn = record_name_of (ty_of e) in
      let (_, _, off) = List.find (fun (n,_,_) -> n = fname) (record_fields rn) in
      let fp = field_ptr base off in
      if is_agg fty then fp else load_ty fty fp

  | TEAssignField (place, fname, value) ->
      let base = lower place in
      let rn = record_name_of (ty_of place) in
      let (_, ft, off) = List.find (fun (n,_,_) -> n = fname) (record_fields rn) in
      let v = lower value in
      let fp = field_ptr base off in
      if is_agg ft then blit v fp (size_of ft)
      else store_ty ft v fp;
      iconst "w" "0"

  | TECtor (c, _, args, ret) ->
      let en = enum_name_of ret in
      let (tag, arglay) = ctor_layout en c in
      let sz = size_of ret in
      let p = alloc sz in
      store_l (iconst "l" (string_of_int tag)) p;   (* tag at offset 0 *)
      List.iter2 (fun a (aty, off) ->
        let v = lower a in
        let fp = field_ptr p off in
        if is_agg aty then blit v fp (size_of aty)
        else store_ty aty v fp) args arglay;
      p

  | TEMatch (scrut, scrut_ty, arms, rty) ->
      let p = lower scrut in
      let agg = is_agg rty in
      let rq = if agg then "l" else qty rty in
      let rslot = alloc 8 in
      let lj = flabel "mjoin" in
      if arms = [] then do_abort ()                  (* absurd *)
      else begin
        let en = (match scrut_ty with A.TyApp (n,_) when is_enum n -> Some n | _ -> None) in
        let tagv = match en with
          | Some _ -> load_l p | None -> "" in
        let single_cmp pat =
          match pat, en with
          | A.PCtor (c, _), Some e ->
              let (tag, _) = ctor_layout e c in
              let tg = iconst "l" (string_of_int tag) in
              let c = fresh () in ins "%s = icmp eq %s, %s" c tagv tg; cmp_to_w c
          | A.PInt n, _ ->
              let nv = iconst (qty scrut_ty) (string_of_int n) in
              let c = fresh () in ins "%s = icmp eq %s, %s" c p nv; cmp_to_w c
          | A.PBool b, _ ->
              let nv = iconst "w" (if b then "1" else "0") in
              let c = fresh () in ins "%s = icmp eq %s, %s" c p nv; cmp_to_w c
          | A.PStr s, _ ->
              let lab = lower (TEStringLit s) in
              let len = iconst "l" (string_of_int (String.length s)) in
              do_call "orto_rt_streq" [("i64", p); ("i64", lab); ("i64", len)] (Some "w")
          | _ -> failwith "clif: unsupported pattern"
        in
        let bind_ctor c vars =
          let e = match en with Some e -> e | None -> failwith "clif: ctor pat on non-enum" in
          let (_, arglay) = ctor_layout e c in
          List.iteri (fun i v ->
            if v <> "_" then begin
              let (aty, off) = List.nth arglay i in
              let fp = field_ptr p off in
              if is_agg aty then Hashtbl.replace locals v (Agg (fp, size_of aty))
              else begin
                let slot = alloc 8 in
                let lv = load_ty aty fp in
                store_ty aty lv slot;
                Hashtbl.replace locals v (Scal (slot, aty))
              end
            end) vars
        in
        let rec go = function
          | [] -> do_abort ()
          | (pat, guard, body) :: rest ->
              let lnext = flabel "marm" and lhit = flabel "mhit" in
              (match pat with
               | A.PBind _ -> jmp lhit
               | A.POr pats ->
                   let conds = List.map single_cmp pats in
                   let cond = List.fold_left (fun acc c ->
                     match acc with "" -> c
                     | x -> let r = fresh () in ins "%s = bor %s, %s" r x c; r) "" conds in
                   brif cond lhit lnext
               | _ -> let c = single_cmp pat in brif c lhit lnext);
              label lhit;
              (match pat with
               | A.PCtor (c, vars) -> bind_ctor c vars
               | A.PBind x when x <> "_" ->
                   if is_agg scrut_ty then Hashtbl.replace locals x (Agg (p, size_of scrut_ty))
                   else begin
                     let slot = alloc 8 in
                     Hashtbl.replace locals x (Scal (slot, scrut_ty));
                     store_ty scrut_ty p slot
                   end
               | _ -> ());
              (match guard with
               | Some g -> let gv = lower g in let lb = flabel "gbody" in
                           brif gv lb lnext; label lb
               | None -> ());
              let v = lower body in
              if agg then store_l v rslot else store_q rq v rslot;
              jmp lj;
              label lnext;
              go rest
        in
        go arms
      end;
      label lj;
      if agg then load_l rslot else load_q rq rslot

  (* ----- calls ----- *)
  | TECall (TEFnRef (name, _, _), args, ret) ->
      let arglist = List.map (fun a -> (ctype (qty (ty_of a)), lower a)) args in
      if is_agg ret then begin
        let sz = size_of ret in
        let p = alloc sz in
        ignore (do_call name (("i64", p) :: arglist) None);
        p
      end else begin
        let rq = qty ret in
        do_call name arglist (Some rq)
      end

  (* ----- closures (M5) ----- *)
  | TEFnRef (name, _, fnty) ->
      Hashtbl.replace wrappers name fnty;
      let p = alloc 16 in
      store_l (iconst "l" "0") p;
      let c8 = field_ptr p 8 in
      let ca = fresh () in ins "%s = func_addr.i64 %s" ca (funcref_of (Printf.sprintf "fnval_%s" name));
      store_l ca c8; p

  | TEMakeClosure (name, _, captures, region_e, _) ->
      let rp = lower region_e in
      let lay = capture_layout captures in
      let envsize = capture_size captures in
      let hbuf = alloc 32 in
      ignore (do_call "orto_rt_ref"
        [("i64", rp); ("i64", iconst "l" "1"); ("i64", iconst "l" (string_of_int envsize));
         ("i64", iconst "l" "0"); ("i64", hbuf)] None);
      let envp = do_call "orto_rt_data" [("i64", hbuf)] (Some "l") in
      List.iter (fun (cn, ct, off) ->
        let v = lower (TEVar (cn, ct)) in
        let fp = field_ptr envp off in
        if is_agg ct then blit v fp (size_of ct)
        else store_ty ct v fp) lay;
      let p = alloc 16 in
      store_l envp p;
      let c8 = field_ptr p 8 in
      let ca = fresh () in ins "%s = func_addr.i64 %s" ca (funcref_of name);
      store_l ca c8; p

  (* indirect call: callee is a fat pointer value {env, code} *)
  | TECall (callee, args, ret) ->
      let fp = lower callee in
      let envp = load_l fp in
      let c8 = field_ptr fp 8 in
      let code = load_l c8 in
      let avs = List.map (fun a -> (ctype (qty (ty_of a)), lower a)) args in
      if is_agg ret then begin
        let sz = size_of ret in
        let sp = alloc sz in
        let argqs = "i64" :: "i64" :: List.map fst avs in
        let sg = get_sig argqs None in
        let argstr = String.concat ", " (envp :: sp :: List.map snd avs) in
        ins "call_indirect %s, %s(%s)" sg code argstr;
        sp
      end else begin
        let rq = qty ret in
        let argqs = "i64" :: List.map fst avs in
        let sg = get_sig argqs (Some rq) in
        let argstr = String.concat ", " (envp :: List.map snd avs) in
        let r = fresh () in
        ins "%s = call_indirect %s, %s(%s)" r sg code argstr; r
      end

  (* ----- regions & handles (M3b) ----- *)
  | TERegion (n, _) | TEStackRegion (n, _) ->
      let nv = lower n in
      let p = alloc 16 in
      ignore (do_call "orto_rt_region" [("i64", nv); ("i64", p)] None); p
  | TEAlignedRegion (n, _a, _) ->
      let nv = lower n in
      let p = alloc 16 in
      ignore (do_call "orto_rt_region" [("i64", nv); ("i64", p)] None); p

  | TEHandle (r, n, init, hty) ->
      let elemt = handle_elem hty in
      let es = size_of elemt in
      let rp = lower r in
      let nv = lower n in
      let iv = lower init in
      let ip = alloc (if es < 8 then 8 else es) in
      if is_agg elemt then blit iv ip es
      else store_ty elemt iv ip;
      let h = alloc 32 in
      ignore (do_call "orto_rt_ref"
        [("i64", rp); ("i64", nv); ("i64", iconst "l" (string_of_int es));
         ("i64", ip); ("i64", h)] None);
      h

  | TEHandleLit (r, elems, hty) ->
      let elemt = handle_elem hty in
      let es = size_of elemt in
      let rp = lower r in
      let n = List.length elems in
      let h = alloc 32 in
      ignore (do_call "orto_rt_ref"
        [("i64", rp); ("i64", iconst "l" (string_of_int n)); ("i64", iconst "l" (string_of_int es));
         ("i64", iconst "l" "0"); ("i64", h)] None);
      List.iteri (fun i el ->
        let ev = lower el in
        let addr = do_call "orto_rt_at"
          [("i64", h); ("i64", iconst "l" (string_of_int i)); ("i64", iconst "l" (string_of_int es))] (Some "l") in
        if is_agg elemt then blit ev addr es
        else store_ty elemt ev addr) elems;
      h

  | TEIndex (a, i, elemt) ->
      let es = size_of elemt in
      let addr =
        match ty_of a with
        | A.TyPtr _ ->
            let av = lower a in let iv = convert (qty (ty_of i)) "l" (lower i) in
            let esv = iconst "l" (string_of_int es) in
            let off = fresh () in ins "%s = imul %s, %s" off iv esv;
            let ad = fresh () in ins "%s = iadd %s, %s" ad av off; ad
        | _ -> region_elem_addr a i es
      in
      if is_agg elemt then addr else load_ty elemt addr

  | TEAssignIdx (a, i, v, _) ->
      let elemt = (match ty_of a with A.TyPtr e -> e | t -> handle_elem t) in
      let es = size_of elemt in
      let addr =
        match ty_of a with
        | A.TyPtr _ ->
            let av = lower a in let iv = convert (qty (ty_of i)) "l" (lower i) in
            let esv = iconst "l" (string_of_int es) in
            let off = fresh () in ins "%s = imul %s, %s" off iv esv;
            let ad = fresh () in ins "%s = iadd %s, %s" ad av off; ad
        | _ -> region_elem_addr a i es
      in
      let vv = lower v in
      if is_agg elemt then blit vv addr es
      else store_ty elemt vv addr;
      iconst "w" "0"

  | TELen (a, _) ->
      let av = lower a in do_call "orto_rt_len" [("i64", av)] (Some "l")

  | TESlice (a, lo, hi, sty) ->
      let elemt = handle_elem sty in let es = size_of elemt in
      let av = lower a in let lov = lower lo in let hiv = lower hi in
      let out = alloc 32 in
      ignore (do_call "orto_rt_slice"
        [("i64", av); ("i64", lov); ("i64", hiv); ("i64", iconst "l" (string_of_int es)); ("i64", out)] None);
      out

  | TEReset r -> let rp = lower r in ignore (do_call "orto_rt_reset" [("i64", rp)] None); iconst "w" "0"

  | TEDrop (e, t) ->
      let v = lower e in
      (match t with
       | A.TyApp ("Region", []) -> ignore (do_call "orto_rt_drop" [("i64", v)] None)
       | _ -> ());
      iconst "w" "0"

  | TEStringLit s ->
      let lab = intern_string s in
      let len = String.length s in
      let dp = string_ptr lab len in
      let h = alloc 32 in
      ignore (do_call "orto_rt_wrap"
        [("i64", dp); ("i64", iconst "l" (string_of_int len)); ("i64", h)] None);
      h

  (* ----- raw pointers ----- *)
  | TECAlloc (elemt, n, _) ->
      let es = size_of elemt in let nv = lower n in
      let esv = iconst "l" (string_of_int es) in
      let bytes = fresh () in ins "%s = imul %s, %s" bytes nv esv;
      do_call "malloc" [("i64", bytes)] (Some "l")
  | TECFree p -> let v = lower p in ignore (do_call "free" [("i64", v)] None); iconst "w" "0"
  | TENullPtr _ -> iconst "l" "0"
  | TEIsNull p -> let v = lower p in let z = iconst "l" "0" in
                  let c = fresh () in ins "%s = icmp eq %s, %s" c v z; cmp_to_w c
  | TEHandleData (h, _) ->
      let v = lower h in do_call "orto_rt_data" [("i64", v)] (Some "l")
  | TEPtrCast (e, _) -> lower e
  | TEDeref (p, elemt) ->
      let v = lower p in load_ty elemt v

  | TETryAt (a, i, optty) ->
      let elemt = handle_elem (ty_of a) in let es = size_of elemt in
      let av = lower a in let iv = lower i in
      let addr = do_call "orto_rt_try"
        [("i64", av); ("i64", iv); ("i64", iconst "l" (string_of_int es))] (Some "l") in
      let en = enum_name_of optty in
      let (some_tag, some_lay) = ctor_layout en "Some" in
      let (none_tag, _) = ctor_layout en "None" in
      let outp = alloc (size_of optty) in
      let lsome = flabel "some" and lnone = flabel "none" and lj = flabel "tj" in
      brif addr lsome lnone;
      label lsome;
        store_l (iconst "l" (string_of_int some_tag)) outp;
        let (aty, off) = List.hd some_lay in
        let fp = field_ptr outp off in
        (if is_agg aty then blit addr fp es
         else (let lv = load_ty aty addr in store_ty aty lv fp));
        jmp lj;
      label lnone; store_l (iconst "l" (string_of_int none_tag)) outp; jmp lj;
      label lj; outp

  | TEPrint (_, exprs, _) -> List.iter (fun e -> ignore (lower e)) exprs; iconst "w" "0"

  (* ----- tuples ----- *)
  | TETuple (es, tty) ->
      let p = alloc (size_of tty) in
      ignore (List.fold_left (fun off e ->
        let ev = lower e in let et = ty_of e in
        let fp = field_ptr p off in
        (if is_agg et then blit ev fp (size_of et)
         else store_ty et ev fp);
        off + size_of et) 0 es);
      p
  | TETupleIdx (e, idx, comp_ty) ->
      let base = lower e in
      let tys = match ty_of e with A.TyTuple ts -> ts | _ -> [] in
      let rec off_of i acc = function
        | _ when i = 0 -> acc
        | x :: rest -> off_of (i-1) (acc + size_of x) rest
        | [] -> acc
      in
      let off = off_of idx 0 tys in
      let fp = field_ptr base off in
      if is_agg comp_ty then fp else load_ty comp_ty fp
  | TELetTuple (names, tty, value, body, _, _) ->
      let p = lower value in
      let tys = match tty with A.TyTuple ts -> ts | _ -> [] in
      ignore (List.fold_left2 (fun off name ct ->
        if name <> "_" then begin
          let fp = field_ptr p off in
          if is_agg ct then Hashtbl.replace locals name (Agg (fp, size_of ct))
          else begin
            let slot = alloc 8 in
            let lv = load_ty ct fp in
            store_ty ct lv slot;
            Hashtbl.replace locals name (Scal (slot, ct))
          end
        end;
        off + size_of ct) 0 names tys);
      lower body

  | e ->
      let tag = match e with
        | TEMakeClosure _ -> "TEMakeClosure" | TECall _ -> "TECall-indirect"
        | TEFnRef _ -> "TEFnRef" | TEAwait _ -> "TEAwait" | TESpawn _ -> "TESpawn"
        | TEForStream _ -> "TEForStream" | TEAwaitAll _ -> "TEAwaitAll"
        | TEFloat _ -> "TEFloat" | _ -> "other"
      in
      failwith (Printf.sprintf "clif: unimplemented %s" tag)

(* compute a field pointer base+off (off may be 0) *)
and field_ptr (base : string) (off : int) : string =
  if off = 0 then base
  else begin
    let ov = iconst "l" (string_of_int off) in
    let r = fresh () in ins "%s = iadd %s, %s" r base ov; r
  end

(* cast a return value to the function's declared return width (handled by
   callers via convert when needed); here just identity *)
and ret_cast (v : string) : string = v

(* checked address of a region-handle element. *)
and region_elem_addr (a : expr) (i : expr) (es : int) : string =
  match a with
  | TEVar (v, _) when Hashtbl.mem hoisted v ->
      let (base, len, _) = Hashtbl.find hoisted v in
      let iv = convert (qty (ty_of i)) "l" (lower i) in
      let z = iconst "l" "0" in
      let blo = fresh () in ins "%s = icmp slt %s, %s" blo iv z;
      let bhi = fresh () in ins "%s = icmp sge %s, %s" bhi iv len;
      let bad = fresh () in ins "%s = bor %s, %s" bad blo bhi;
      let lbad = flabel "hbad" and lok = flabel "hok" in
      brif bad lbad lok;
      label lbad; do_abort ();
      label lok;
      let esv = iconst "l" (string_of_int es) in
      let off = fresh () in ins "%s = imul %s, %s" off iv esv;
      let ad = fresh () in ins "%s = iadd %s, %s" ad base off; ad
  | _ ->
      let av = lower a in let iv = convert (qty (ty_of i)) "l" (lower i) in
      do_call "orto_rt_at" [("i64", av); ("i64", iv); ("i64", iconst "l" (string_of_int es))] (Some "l")

(* materialize a string's bytes at runtime: malloc + per-byte stores, return ptr.
   CLIF (as parsed here) has no data section, so strings are built dynamically. *)
and string_ptr (_lab : string) (len : int) : string =
  let s = match Hashtbl.fold (fun str lab acc -> if lab = _lab then Some str else acc) strings None with
    | Some str -> str | None -> "" in
  let dp = do_call "malloc" [("i64", iconst "l" (string_of_int (len + 1)))] (Some "l") in
  String.iteri (fun idx ch ->
    let p = field_ptr dp idx in
    ins "istore8 %s, %s" (iconst "w" (string_of_int (Char.code ch))) p) s;
  let nul = field_ptr dp len in
  ins "istore8 %s, %s" (iconst "w" "0") nul;
  dp

(* a funcref to a defined/imported function %name; signature filled in lazily
   via get_fn when the function is actually CALLED. For func_addr we need a
   funcref decl, so emit one with a placeholder signature inferred elsewhere. *)
and funcref_of (name : string) : string =
  (* func_addr needs an fnN decl. We don't know the precise sig here, but the
     wrapper signature is consistent; declare with an opaque (i64...) sig that
     matches how the wrapper is later invoked indirectly. We reuse get_fn with
     a generic single-arg-returning-i64 sig is wrong; instead, declare the
     funcref against the closure-call sig used at the indirect call site.
     Simplest correct approach: emit the funcref with the env-call signature
     ( (i64) -> i64 ) is not always right either. We instead store the address
     and rely on call_indirect supplying the true sig. So just need *a* funcref
     whose name is %name; its declared sig is unused for func_addr. *)
  get_fn name (get_sig ["i64"] (Some "l"))

(* ===== a function ===== *)
let emit_func (f : func) : string =
  Buffer.clear done_blocks; Buffer.clear cur; Buffer.clear entry_buf; Buffer.clear pre_buf;
  tmp := -1; blk := 0; slot_n := 0; sig_n := 0; fn_n := 0;
  Hashtbl.clear sig_tbl; Hashtbl.clear fn_tbl;
  Hashtbl.clear locals; loops := []; Hashtbl.clear hoisted;
  cur_term := false; first_block := true;
  let agg_ret = is_agg f.return_ty && f.name <> "main" in
  cur_sret := None; cur_ret_size := 0;
  cur_ret_q := (if f.name = "main" then "w" else qty f.return_ty);
  (* parameter value names: v0,v1,... in declaration order. We assign them now,
     before any fresh() for the body, so they line up with the signature. *)
  let pcount = ref 0 in
  let next_param () = let v = Printf.sprintf "v%d" !pcount in incr pcount; v in
  (* env / sret / params, in the order they appear in the signature *)
  let env_v = if f.takes_env then Some (next_param ()) else None in
  let sret_v = if agg_ret then (cur_sret := Some (next_param ()); cur_ret_size := size_of f.return_ty;
                                (match !cur_sret with Some s -> Some s | None -> None)) else None in
  let param_vs = List.map (fun (name, t) ->
    let v = next_param () in (name, t, v)) f.params in
  (* the body's fresh values must not collide with parameter value names *)
  tmp := !pcount - 1;
  (* the entry block (block0) declares the params as block params; the function
     signature lists only types. *)
  let blk0_params =
    let parts = ref [] in
    (match env_v with Some ev -> parts := !parts @ [Printf.sprintf "%s: i64" ev] | None -> ());
    (match sret_v with Some s -> parts := !parts @ [Printf.sprintf "%s: i64" s] | None -> ());
    List.iter (fun (_, t, v) -> parts := !parts @ [Printf.sprintf "%s: %s" v (ctype (qty t))]) param_vs;
    String.concat ", " !parts
  in
  cur_name := (if blk0_params = "" then "block0" else Printf.sprintf "block0(%s)" blk0_params);
  cur_name_block0 := !cur_name;
  (* now bind params into slots (entry block) *)
  List.iter (fun (name, t, v) ->
    if is_agg t then Hashtbl.replace locals name (Agg (v, size_of t))
    else begin
      let slot = alloc 8 in
      store_ty t v slot;
      Hashtbl.replace locals name (Scal (slot, t))
    end) param_vs;
  (* bind env captures *)
  (match env_v with
   | Some ev ->
       List.iter (fun (cn, ct, off) ->
         let fp = field_ptr ev off in
         if is_agg ct then Hashtbl.replace locals cn (Agg (fp, size_of ct))
         else begin
           let slot = alloc 8 in
           let lv = load_ty ct fp in
           store_ty ct lv slot;
           Hashtbl.replace locals cn (Scal (slot, ct))
         end) (capture_layout f.captures)
   | None -> ());
  let v = lower f.body in
  (* final terminator *)
  if f.name = "main" then begin
    let rv = convert (lowered_q f.body) "w" v in
    ins "return %s" rv; cur_term := true
  end else if agg_ret then begin
    (match sret_v with Some s -> blit v s !cur_ret_size | None -> ());
    ins "return"; cur_term := true
  end else begin
    let rv = convert (lowered_q f.body) (qty f.return_ty) v in
    ins "return %s" rv; cur_term := true
  end;
  flush_cur ();
  (* signature string: types only *)
  let pstr =
    let parts = ref [] in
    (match env_v with Some _ -> parts := !parts @ ["i64"] | None -> ());
    (match sret_v with Some _ -> parts := !parts @ ["i64"] | None -> ());
    List.iter (fun (_, t, _) -> parts := !parts @ [ctype (qty t)]) param_vs;
    String.concat ", " !parts
  in
  let ret_clause =
    if f.name = "main" then " -> i32"
    else if agg_ret then ""
    else " -> " ^ ctype (qty f.return_ty)
  in
  (* inject entry-block instructions (stack_addr etc.) right after block0:,
     which is always the first thing emitted into done_blocks *)
  let blocks_txt =
    let s = Buffer.contents done_blocks in
    let marker = !cur_name_block0 ^ ":\n" in
    let ml = String.length marker in
    if String.length s >= ml && String.sub s 0 ml = marker then
      marker ^ Buffer.contents entry_buf ^ String.sub s ml (String.length s - ml)
    else s
  in
  Printf.sprintf "function %%%s(%s)%s system_v {\n%s\n%s}\n"
    f.name pstr ret_clause (Buffer.contents pre_buf) blocks_txt

let emit (prog : program) : string =
  Hashtbl.clear records; Hashtbl.clear enums; Hashtbl.clear strings; Hashtbl.clear wrappers;
  List.iter (fun (rd : A.record_decl) -> Hashtbl.replace records rd.A.rec_name rd) prog.records;
  List.iter (fun (td : A.type_decl) -> Hashtbl.replace enums td.A.type_name td) prog.types;
  compute_region_safe prog.funcs;
  let fns = String.concat "\n" (List.map emit_func prog.funcs) in
  (* env-adapting wrappers for plain fns used as values (TEFnRef). Emitted as
     standalone CLIF functions named %fnval_NAME. *)
  let wraps =
    Hashtbl.fold (fun name fnty acc ->
      let (args, ret) = match fnty with A.TyFun (a, r) -> (a, r) | _ -> ([], A.TyInt) in
      let agg = is_agg ret in
      (* fresh per-wrapper value/preamble state *)
      Buffer.clear pre_buf; Hashtbl.clear sig_tbl; Hashtbl.clear fn_tbl;
      sig_n := 0; fn_n := 0;
      (* parameter values: env, [sret], args... *)
      let vi = ref 0 in
      let nv () = let v = Printf.sprintf "v%d" !vi in incr vi; v in
      let env_v = nv () in
      let sret_v = if agg then Some (nv ()) else None in
      let arg_vs = List.map (fun t -> (t, nv ())) args in
      tmp := !vi - 1;
      let body = Buffer.create 128 in
      let emit_line s = Buffer.add_string body ("    " ^ s ^ "\n") in
      let argqs = List.map (fun (t, _) -> ctype (qty t)) arg_vs in
      let argvals = List.map snd arg_vs in
      if agg then begin
        let s = match sret_v with Some s -> s | None -> "" in
        let sg = get_sig ("i64" :: argqs) None in
        let fn = get_fn name sg in
        emit_line (Printf.sprintf "call %s(%s)" fn (String.concat ", " (s :: argvals)));
        emit_line "return"
      end else begin
        let rq = qty ret in
        let sg = get_sig argqs (Some rq) in
        let fn = get_fn name sg in
        let rv = nv () in
        emit_line (Printf.sprintf "%s = call %s(%s)" rv fn (String.concat ", " argvals));
        emit_line (Printf.sprintf "return %s" rv)
      end;
      (* function signature: types only *)
      let sigtys =
        let parts = ref ["i64"] in
        (match sret_v with Some _ -> parts := !parts @ ["i64"] | None -> ());
        List.iter (fun q -> parts := !parts @ [q]) argqs;
        String.concat ", " !parts
      in
      (* entry block params: env, [sret], args *)
      let blkparams =
        let parts = ref [Printf.sprintf "%s: i64" env_v] in
        (match sret_v with Some s -> parts := !parts @ [Printf.sprintf "%s: i64" s] | None -> ());
        List.iter2 (fun q v -> parts := !parts @ [Printf.sprintf "%s: %s" v q]) argqs argvals;
        String.concat ", " !parts
      in
      let ret_clause = if agg then "" else " -> " ^ ctype (qty ret) in
      Printf.sprintf "function %%fnval_%s(%s)%s system_v {\n%sblock0(%s):\n%s}\n"
        name sigtys ret_clause (Buffer.contents pre_buf) blkparams (Buffer.contents body) :: acc)
      wrappers []
  in
  String.concat "\n" (wraps @ [fns])

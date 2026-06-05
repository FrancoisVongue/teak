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

(* ===== QBE base type of a scalar orto type ===== *)
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

let store_op = function "w"->"storew" | "d"->"stored" | "s"->"stores" | _->"storel"
let load_op  = function "w"->"loadw"  | "d"->"loadd"  | "s"->"loads"  | _->"loadl"

(* width-correct store/load for a scalar of a given orto type *)
let store_ty (t : A.ty) : string =
  match scalar_size t, qty t with
  | 1, _ -> "storeb" | 2, _ -> "storeh"
  | 4, "s" -> "stores" | 4, _ -> "storew"
  | _, "d" -> "stored" | _, "s" -> "stores" | _ -> "storel"
let load_ty (t : A.ty) : string =
  match scalar_size t, qty t with
  | 1, _ -> "loadub" | 2, _ -> "loaduh"
  | 4, "s" -> "loads" | 4, _ -> "loadw"
  | _, "d" -> "loadd" | _, "s" -> "loads" | _ -> "loadl"

(* ===== per-function emit state ===== *)
let buf = Buffer.create 1024
let slots_buf = Buffer.create 256   (* alloc8 lines, hoisted to @start (never inside a loop) *)
let tmp = ref 0
let lbl = ref 0
let fresh () = incr tmp; Printf.sprintf "%%.t%d" !tmp
let flabel p = incr lbl; Printf.sprintf "@.%s%d" p !lbl
let ins fmt = Printf.ksprintf (fun s -> Buffer.add_string buf ("\t" ^ s ^ "\n")) fmt
(* alloc instructions go here so they execute once at entry, not per loop iteration *)
let ains fmt = Printf.ksprintf (fun s -> Buffer.add_string slots_buf ("\t" ^ s ^ "\n")) fmt
let label l = Buffer.add_string buf (l ^ "\n")
let term fmt =
  Printf.ksprintf (fun s ->
    Buffer.add_string buf ("\t" ^ s ^ "\n");
    label (flabel "dead")) fmt

type local = Scal of string * A.ty | Agg of string * int
let locals : (string, local) Hashtbl.t = Hashtbl.create 16
let loops : (string * string) list ref = ref []
let cur_sret : string option ref = ref None     (* Some ptr if fn returns an aggregate *)
let cur_ret_size : int ref = ref 0

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
  | TEBorrow (_, t) -> t
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
  | TEBorrow (a, _) -> [a]
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

(* ===== numeric conversion ===== *)
let convert (sq : string) (dq : string) (v : string) : string =
  if sq = dq then v
  else begin
    let r = fresh () in
    (match sq, dq with
     | "w", "l"       -> ins "%s =l extsw %s" r v
     | "l", "w"       -> ins "%s =w copy %s" r v
     | ("w"|"l"), "d" -> ins "%s =d s%stof %s" r sq v
     | ("w"|"l"), "s" -> ins "%s =s s%stof %s" r sq v
     | "d", ("w"|"l") -> ins "%s =%s dtosi %s" r dq v
     | "s", ("w"|"l") -> ins "%s =%s stosi %s" r dq v
     | "s", "d"       -> ins "%s =d exts %s" r v
     | "d", "s"       -> ins "%s =s truncd %s" r v
     | _              -> ins "%s =%s copy %s" r dq v);
    r
  end

(* ===== lower an expression to a QBE operand ===== *)
let rec lower (e : expr) : string =
  match e with
  | TEInt n  -> Int64.to_string n
  | TEBool b -> if b then "1" else "0"
  | TEFloat f -> Printf.sprintf "d_%.17g" f

  | TEVar (name, _) ->
      (match Hashtbl.find locals name with
       | Agg (ptr, _) -> ptr                       (* value of an aggregate = its address *)
       | Scal (slot, ty) -> let q = qty ty in let r = fresh () in ins "%s =%s %s %s" r q (load_ty ty) slot; r)

  | TELet (name, vty, value, body, _, _) ->
      if is_agg vty then begin
        (* copy the aggregate so the binding is independent (value semantics);
           e.g. `let t = xs[i]` must not alias the buffer cell. *)
        let v = lower value in
        let sz = size_of vty in
        let p = fresh () in ains "%s =l alloc8 %d" p sz;
        ins "blit %s, %s, %d" v p sz;
        Hashtbl.replace locals name (Agg (p, sz))
      end else begin
        let v = lower value in
        let slot = fresh () in
        ains "%s =l alloc8 8" slot;
        ins "%s %s, %s" (store_ty vty) v slot;
        Hashtbl.replace locals name (Scal (slot, vty))
      end;
      lower body

  | TEAssign (name, value, _) ->
      (match Hashtbl.find locals name with
       | Scal (slot, ty) -> let v = lower value in ins "%s %s, %s" (store_ty ty) v slot
       | Agg (ptr, sz)  -> let v = lower value in ins "blit %s, %s, %d" v ptr sz);
      "0"

  (* pointer arithmetic: p ± n advances by n elements; p - q is element diff *)
  | TEBinop ((A.OpAdd | A.OpSub) as op, a, b, _)
    when (match ty_of a with A.TyPtr _ -> true | _ -> false)
      && (match ty_of b with A.TyPtr _ -> false | _ -> true) ->
      let es = (match ty_of a with A.TyPtr e -> size_of e | _ -> 1) in
      let pv = lower a in let nv = lower b in
      let scaled = fresh () in ins "%s =l mul %s, %d" scaled nv es;
      let r = fresh () in
      ins "%s =l %s %s, %s" r (if op = A.OpAdd then "add" else "sub") pv scaled; r
  | TEBinop (A.OpSub, a, b, _)
    when (match ty_of a, ty_of b with A.TyPtr _, A.TyPtr _ -> true | _ -> false) ->
      let es = (match ty_of a with A.TyPtr e -> size_of e | _ -> 1) in
      let pa = lower a in let pb = lower b in
      let d = fresh () in ins "%s =l sub %s, %s" d pa pb;
      let r = fresh () in ins "%s =l div %s, %d" r d es; r

  (* short-circuit && and || *)
  | TEBinop (A.OpAnd, a, b, _) ->
      let rslot = fresh () in ains "%s =l alloc8 8" rslot;
      let va = lower a in
      let lb = flabel "andb" and lf = flabel "andf" and le = flabel "ande" in
      ins "jnz %s, %s, %s" va lb lf;
      label lb; let vb = lower b in ins "storew %s, %s" vb rslot; ins "jmp %s" le;
      label lf; ins "storew 0, %s" rslot; ins "jmp %s" le;
      label le; let r = fresh () in ins "%s =w loadw %s" r rslot; r
  | TEBinop (A.OpOr, a, b, _) ->
      let rslot = fresh () in ains "%s =l alloc8 8" rslot;
      let va = lower a in
      let lt = flabel "ort" and lb = flabel "orb" and le = flabel "ore" in
      ins "jnz %s, %s, %s" va lt lb;
      label lt; ins "storew 1, %s" rslot; ins "jmp %s" le;
      label lb; let vb = lower b in ins "storew %s, %s" vb rslot; ins "jmp %s" le;
      label le; let r = fresh () in ins "%s =w loadw %s" r rslot; r

  | TEBinop (op, a, b, t) ->
      let va = lower a in
      let vb = lower b in
      let aq = qty (ty_of a) in
      let rq = qty t in
      let cp = if aq = "d" || aq = "s" then "" else "s" in   (* float: no signed prefix *)
      let r = fresh () in
      (match op with
       | A.OpAdd  -> ins "%s =%s add %s, %s"  r rq va vb
       | A.OpSub  -> ins "%s =%s sub %s, %s"  r rq va vb
       | A.OpMul  -> ins "%s =%s mul %s, %s"  r rq va vb
       | A.OpDiv  -> ins "%s =%s div %s, %s"  r rq va vb
       | A.OpMod  -> ins "%s =%s rem %s, %s"  r rq va vb
       | A.OpBAnd | A.OpAnd -> ins "%s =%s and %s, %s" r rq va vb
       | A.OpBOr  | A.OpOr  -> ins "%s =%s or %s, %s"  r rq va vb
       | A.OpBXor -> ins "%s =%s xor %s, %s"  r rq va vb
       | A.OpShl  -> ins "%s =%s shl %s, %s"  r rq va vb
       | A.OpShr  -> ins "%s =%s sar %s, %s"  r rq va vb
       (* float comparisons have no signed/unsigned prefix (cltd, not csltd) *)
       | A.OpEq   -> ins "%s =w ceq%s %s, %s"  r aq va vb
       | A.OpNeq  -> ins "%s =w cne%s %s, %s"  r aq va vb
       | A.OpLt   -> ins "%s =w c%slt%s %s, %s" r cp aq va vb
       | A.OpLe   -> ins "%s =w c%sle%s %s, %s" r cp aq va vb
       | A.OpGt   -> ins "%s =w c%sgt%s %s, %s" r cp aq va vb
       | A.OpGe   -> ins "%s =w c%sge%s %s, %s" r cp aq va vb);
      r

  | TEUnop (op, a, t) ->
      let va = lower a in
      let r = fresh () in
      (match op with
       | A.OpNeg  -> ins "%s =%s sub 0, %s" r (qty t) va
       | A.OpBNot -> ins "%s =%s xor %s, -1" r (qty t) va
       | A.OpNot  -> ins "%s =w ceqw %s, 0" r va);
      r

  | TEIf (c, th, el, t) ->
      let agg = is_agg t in
      let q = qty t in
      let rslot = fresh () in
      if agg then ains "%s =l alloc8 8" rslot else ains "%s =l alloc8 8" rslot;
      let lt = flabel "then" and le = flabel "else" and lj = flabel "join" in
      let vc = lower c in
      ins "jnz %s, %s, %s" vc lt le;
      label lt;
      let vt = lower th in ins "%s %s, %s" (if agg then "storel" else store_ty t) vt rslot; ins "jmp %s" lj;
      label le;
      let ve = lower el in ins "%s %s, %s" (if agg then "storel" else store_ty t) ve rslot; ins "jmp %s" lj;
      label lj;
      let r = fresh () in ins "%s =%s %s %s" r (if agg then "l" else q) (if agg then "loadl" else load_ty t) rslot; r

  | TEWhile (c, body) ->
      (* candidate loop-invariant handles to hoist (LICM) *)
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
        ins "jmp %s" lc;
        label lc; let vc = lower c in ins "jnz %s, %s, %s" vc lb le;
        label lb;
        loops := (lc, le) :: !loops;
        let _ = lower body in
        loops := List.tl !loops;
        ins "jmp %s" lc;
        label le; "0"
      end else begin
        (* guarded preheader: resolve handles once, only if the loop runs ≥1×
           (so a never-entered loop never spuriously aborts on a dead region) *)
        let lpre = flabel "pre" and lc = flabel "loop"
        and lb = flabel "body" and le = flabel "end" in
        let vc0 = lower c in ins "jnz %s, %s, %s" vc0 lpre le;
        label lpre;
        let added = List.map (fun (v, t) ->
          let es = size_of (handle_elem t) in
          let hv = lower (TEVar (v, t)) in
          let base = fresh () in ins "%s =l call $orto_rt_data(l %s)" base hv;
          let len = fresh () in ins "%s =l call $orto_rt_len(l %s)" len hv;
          Hashtbl.replace hoisted v (base, len, es); v) hoist in
        ins "jmp %s" lb;
        label lb;
        loops := (lc, le) :: !loops;
        let _ = lower body in
        loops := List.tl !loops;
        ins "jmp %s" lc;
        label lc; let vc = lower c in ins "jnz %s, %s, %s" vc lb le;
        label le;
        List.iter (Hashtbl.remove hoisted) added;
        "0"
      end

  | TEBreak    -> let (_, b) = List.hd !loops in term "jmp %s" b; "0"
  | TEContinue -> let (c, _) = List.hd !loops in term "jmp %s" c; "0"

  | TEReturn (v, _) ->
      (match !cur_sret with
       | Some s -> let p = lower v in ins "blit %s, %s, %d" p s !cur_ret_size; term "ret"
       | None   -> let vv = lower v in term "ret %s" vv);
      "0"

  | TEToInt e          -> convert (qty (ty_of e)) "l" (lower e)
  | TEToIntFromFloat e -> convert (qty (ty_of e)) "l" (lower e)
  | TEToFloat e        -> convert (qty (ty_of e)) "d" (lower e)
  | TEToByte e ->
      let v = lower e in let r = fresh () in ins "%s =w and %s, 255" r v; r
  | TECast (target, e) ->
      convert (qty (ty_of e)) (qty (A.TyApp (target, []))) (lower e)

  (* ----- aggregates ----- *)
  | TERecord (name, _, fields, _) ->
      let sz = size_of (A.TyApp (name, [])) in
      let p = fresh () in ains "%s =l alloc8 %d" p sz;
      let layout = record_fields name in
      List.iter (fun (fn, ft, off) ->
        let value = List.assoc fn fields in
        let v = lower value in
        let fp = fresh () in ins "%s =l add %s, %d" fp p off;
        if is_agg ft then ins "blit %s, %s, %d" v fp (size_of ft)
        else ins "%s %s, %s" (store_ty ft) v fp) layout;
      p

  | TEField (e, fname, fty) ->
      let base = lower e in
      let rn = record_name_of (ty_of e) in
      let (_, _, off) = List.find (fun (n,_,_) -> n = fname) (record_fields rn) in
      let fp = fresh () in ins "%s =l add %s, %d" fp base off;
      if is_agg fty then fp
      else (let r = fresh () in ins "%s =%s %s %s" r (qty fty) (load_ty fty) fp; r)

  | TEAssignField (place, fname, value) ->
      let base = lower place in
      let rn = record_name_of (ty_of place) in
      let (_, ft, off) = List.find (fun (n,_,_) -> n = fname) (record_fields rn) in
      let v = lower value in
      let fp = fresh () in ins "%s =l add %s, %d" fp base off;
      if is_agg ft then ins "blit %s, %s, %d" v fp (size_of ft)
      else ins "%s %s, %s" (store_ty ft) v fp;
      "0"

  | TECtor (c, _, args, ret) ->
      let en = enum_name_of ret in
      let (tag, arglay) = ctor_layout en c in
      let sz = size_of ret in
      let p = fresh () in ains "%s =l alloc8 %d" p sz;
      ins "storel %d, %s" tag p;        (* tag at offset 0 *)
      List.iter2 (fun a (aty, off) ->
        let v = lower a in
        let fp = fresh () in ins "%s =l add %s, %d" fp p off;
        if is_agg aty then ins "blit %s, %s, %d" v fp (size_of aty)
        else ins "%s %s, %s" (store_ty aty) v fp) args arglay;
      p

  | TEMatch (scrut, scrut_ty, arms, rty) ->
      let p = lower scrut in
      let agg = is_agg rty in
      let rq = if agg then "l" else qty rty in
      let rslot = fresh () in ains "%s =l alloc8 8" rslot;
      let lj = flabel "mjoin" in
      if arms = [] then (term "call $abort()")     (* absurd *)
      else begin
        let en = (match scrut_ty with A.TyApp (n,_) when is_enum n -> Some n | _ -> None) in
        let tagv = match en with
          | Some _ -> let t = fresh () in ins "%s =l loadl %s" t p; t | None -> "" in
        (* boolean test (w 1/0) for a non-binding pattern *)
        let single_cmp pat =
          match pat, en with
          | A.PCtor (c, _), Some e ->
              let (tag, _) = ctor_layout e c in
              let r = fresh () in ins "%s =w ceql %s, %d" r tagv tag; r
          | A.PInt n, _ ->
              let r = fresh () in ins "%s =w ceq%s %s, %d" r (qty scrut_ty) p n; r
          | A.PBool b, _ ->
              let r = fresh () in ins "%s =w ceqw %s, %d" r p (if b then 1 else 0); r
          | A.PStr s, _ ->
              let lab = intern_string s in
              let r = fresh () in
              ins "%s =w call $orto_rt_streq(l %s, l %s, l %d)" r p lab (String.length s); r
          | _ -> failwith "qbe: unsupported pattern"
        in
        let bind_ctor c vars =
          let e = match en with Some e -> e | None -> failwith "qbe: ctor pat on non-enum" in
          let (_, arglay) = ctor_layout e c in
          List.iteri (fun i v ->
            if v <> "_" then begin
              let (aty, off) = List.nth arglay i in
              let fp = fresh () in ins "%s =l add %s, %d" fp p off;
              if is_agg aty then Hashtbl.replace locals v (Agg (fp, size_of aty))
              else begin
                let slot = fresh () in ains "%s =l alloc8 8" slot;
                let q = qty aty in
                let lv = fresh () in ins "%s =%s %s %s" lv q (load_ty aty) fp;
                ins "%s %s, %s" (store_ty aty) lv slot;
                Hashtbl.replace locals v (Scal (slot, aty))
              end
            end) vars
        in
        let rec go = function
          | [] -> term "call $abort()"
          | (pat, guard, body) :: rest ->
              let lnext = flabel "marm" and lhit = flabel "mhit" in
              (match pat with
               | A.PBind _ -> ins "jmp %s" lhit
               | A.POr pats ->
                   let conds = List.map single_cmp pats in
                   let cond = List.fold_left (fun acc c ->
                     match acc with "" -> c
                     | x -> let r = fresh () in ins "%s =w or %s, %s" r x c; r) "" conds in
                   ins "jnz %s, %s, %s" cond lhit lnext
               | _ -> let c = single_cmp pat in ins "jnz %s, %s, %s" c lhit lnext);
              label lhit;
              (match pat with
               | A.PCtor (c, vars) -> bind_ctor c vars
               | A.PBind x when x <> "_" ->
                   if is_agg scrut_ty then Hashtbl.replace locals x (Agg (p, size_of scrut_ty))
                   else begin
                     let slot = fresh () in ains "%s =l alloc8 8" slot;
                     Hashtbl.replace locals x (Scal (slot, scrut_ty));
                     ins "%s %s, %s" (store_ty scrut_ty) p slot
                   end
               | _ -> ());
              (match guard with
               | Some g -> let gv = lower g in let lb = flabel "gbody" in
                           ins "jnz %s, %s, %s" gv lb lnext; label lb
               | None -> ());
              let v = lower body in
              ins "%s %s, %s" (store_op rq) v rslot;
              ins "jmp %s" lj;
              label lnext;
              go rest
        in
        go arms
      end;
      label lj;
      let r = fresh () in ins "%s =%s %s %s" r rq (load_op rq) rslot; r

  (* ----- calls ----- *)
  | TECall (TEFnRef (name, _, _), args, ret) ->
      let avs = List.map (fun a -> (qty (ty_of a), lower a)) args in
      let argstr = String.concat ", " (List.map (fun (q, v) -> Printf.sprintf "%s %s" q v) avs) in
      if is_agg ret then begin
        (* sret: caller allocates, passes hidden ptr first, callee fills it *)
        let sz = size_of ret in
        let p = fresh () in ains "%s =l alloc8 %d" p sz;
        let argstr = if argstr = "" then Printf.sprintf "l %s" p
                     else Printf.sprintf "l %s, %s" p argstr in
        ins "call $%s(%s)" name argstr;
        p
      end else begin
        let rq = qty ret in
        let r = fresh () in
        ins "%s =%s call $%s(%s)" r rq name argstr;
        r
      end

  (* ----- closures (M5) ----- *)
  | TEFnRef (name, _, fnty) ->
      (* a plain fn as a value: fat pointer {env=0, code=wrapper} *)
      Hashtbl.replace wrappers name fnty;
      let p = fresh () in ains "%s =l alloc8 16" p;
      ins "storel 0, %s" p;
      let c8 = fresh () in ins "%s =l add %s, 8" c8 p;
      let ca = fresh () in ins "%s =l copy $fnval_%s" ca name;
      ins "storel %s, %s" ca c8; p

  | TEMakeClosure (name, _, captures, region_e, _) ->
      let rp = lower region_e in
      let lay = capture_layout captures in
      let envsize = capture_size captures in
      let hbuf = fresh () in ains "%s =l alloc8 32" hbuf;
      ins "call $orto_rt_ref(l %s, l 1, l %d, l 0, l %s)" rp envsize hbuf;
      let envp = fresh () in ins "%s =l call $orto_rt_data(l %s)" envp hbuf;
      List.iter (fun (cn, ct, off) ->
        let v = lower (TEVar (cn, ct)) in
        let fp = fresh () in ins "%s =l add %s, %d" fp envp off;
        if is_agg ct then ins "blit %s, %s, %d" v fp (size_of ct)
        else ins "%s %s, %s" (store_ty ct) v fp) lay;
      let p = fresh () in ains "%s =l alloc8 16" p;
      ins "storel %s, %s" envp p;
      let c8 = fresh () in ins "%s =l add %s, 8" c8 p;
      let ca = fresh () in ins "%s =l copy $%s" ca name;
      ins "storel %s, %s" ca c8; p

  (* indirect call: callee is a fat pointer value {env, code} *)
  | TECall (callee, args, ret) ->
      let fp = lower callee in
      let envp = fresh () in ins "%s =l loadl %s" envp fp;
      let c8 = fresh () in ins "%s =l add %s, 8" c8 fp;
      let code = fresh () in ins "%s =l loadl %s" code c8;
      let avs = List.map (fun a -> (qty (ty_of a), lower a)) args in
      let tail = List.map (fun (q, v) -> Printf.sprintf "%s %s" q v) avs in
      if is_agg ret then begin
        let sz = size_of ret in
        let sp = fresh () in ains "%s =l alloc8 %d" sp sz;
        let argstr = String.concat ", " ((Printf.sprintf "l %s" envp) :: (Printf.sprintf "l %s" sp) :: tail) in
        ins "call %s(%s)" code argstr; sp
      end else begin
        let argstr = String.concat ", " ((Printf.sprintf "l %s" envp) :: tail) in
        let rq = qty ret in let r = fresh () in
        ins "%s =%s call %s(%s)" r rq code argstr; r
      end

  (* ----- regions & handles (M3b) ----- *)
  | TERegion (n, _) | TEStackRegion (n, _) ->
      let nv = lower n in
      let p = fresh () in ains "%s =l alloc8 16" p;
      ins "call $orto_rt_region(l %s, l %s)" nv p; p
  | TEAlignedRegion (n, _a, _) ->
      let nv = lower n in
      let p = fresh () in ains "%s =l alloc8 16" p;
      ins "call $orto_rt_region(l %s, l %s)" nv p; p

  | TEHandle (r, n, init, hty) ->
      let elemt = handle_elem hty in
      let es = size_of elemt in
      let rp = lower r in
      let nv = lower n in
      let iv = lower init in
      let ip = fresh () in ains "%s =l alloc8 %d" ip (if es < 8 then 8 else es);
      if is_agg elemt then ins "blit %s, %s, %d" iv ip es
      else ins "%s %s, %s" (store_ty elemt) iv ip;
      let h = fresh () in ains "%s =l alloc8 32" h;
      ins "call $orto_rt_ref(l %s, l %s, l %d, l %s, l %s)" rp nv es ip h;
      h

  | TEHandleLit (r, elems, hty) ->
      let elemt = handle_elem hty in
      let es = size_of elemt in
      let rp = lower r in
      let n = List.length elems in
      let h = fresh () in ains "%s =l alloc8 32" h;
      ins "call $orto_rt_ref(l %s, l %d, l %d, l 0, l %s)" rp n es h;
      List.iteri (fun i el ->
        let ev = lower el in
        let addr = fresh () in ins "%s =l call $orto_rt_at(l %s, l %d, l %d)" addr h i es;
        if is_agg elemt then ins "blit %s, %s, %d" ev addr es
        else ins "%s %s, %s" (store_ty elemt) ev addr) elems;
      h

  | TEIndex (a, i, elemt) ->
      let es = size_of elemt in
      let addr =
        match ty_of a with
        | A.TyPtr _ ->
            let av = lower a in let iv = lower i in
            let off = fresh () in ins "%s =l mul %s, %d" off iv es;
            let ad = fresh () in ins "%s =l add %s, %s" ad av off; ad
        | _ -> region_elem_addr a i es
      in
      if is_agg elemt then addr
      else (let r = fresh () in ins "%s =%s %s %s" r (qty elemt) (load_ty elemt) addr; r)

  | TEAssignIdx (a, i, v, _) ->
      let elemt = (match ty_of a with A.TyPtr e -> e | t -> handle_elem t) in
      let es = size_of elemt in
      let addr =
        match ty_of a with
        | A.TyPtr _ ->
            let av = lower a in let iv = lower i in
            let off = fresh () in ins "%s =l mul %s, %d" off iv es;
            let ad = fresh () in ins "%s =l add %s, %s" ad av off; ad
        | _ -> region_elem_addr a i es
      in
      let vv = lower v in
      if is_agg elemt then ins "blit %s, %s, %d" vv addr es
      else ins "%s %s, %s" (store_ty elemt) vv addr;
      "0"

  | TELen (a, _) ->
      let av = lower a in let r = fresh () in ins "%s =l call $orto_rt_len(l %s)" r av; r

  | TESlice (a, lo, hi, sty) ->
      let elemt = handle_elem sty in let es = size_of elemt in
      let av = lower a in let lov = lower lo in let hiv = lower hi in
      let out = fresh () in ains "%s =l alloc8 32" out;
      ins "call $orto_rt_slice(l %s, l %s, l %s, l %d, l %s)" av lov hiv es out; out

  | TEReset r -> let rp = lower r in ins "call $orto_rt_reset(l %s)" rp; "0"

  | TEDrop (e, t) ->
      let v = lower e in
      (match t with
       | A.TyApp ("Region", []) -> ins "call $orto_rt_drop(l %s)" v
       | _ -> ());   (* user-resource drops: side effect only, no exit-code impact *)
      "0"

  | TEStringLit s ->
      let lab = intern_string s in
      let len = String.length s in
      let h = fresh () in ains "%s =l alloc8 32" h;
      ins "call $orto_rt_wrap(l %s, l %d, l %s)" lab len h; h

  (* ----- raw pointers ----- *)
  | TECAlloc (elemt, n, _) ->
      let es = size_of elemt in let nv = lower n in
      let bytes = fresh () in ins "%s =l mul %s, %d" bytes nv es;
      let r = fresh () in ins "%s =l call $malloc(l %s)" r bytes; r
  | TECFree p -> let v = lower p in ins "call $free(l %s)" v; "0"
  | TENullPtr _ -> "0"
  | TEIsNull p -> let v = lower p in let r = fresh () in ins "%s =w ceql %s, 0" r v; r
  | TEHandleData (h, _) ->
      let v = lower h in let r = fresh () in ins "%s =l call $orto_rt_data(l %s)" r v; r
  | TEPtrCast (e, _) -> lower e
  | TEDeref (p, elemt) ->
      let v = lower p in let r = fresh () in
      ins "%s =%s %s %s" r (qty elemt) (load_ty elemt) v; r

  | TETryAt (a, i, optty) ->
      let elemt = handle_elem (ty_of a) in let es = size_of elemt in
      let av = lower a in let iv = lower i in
      let addr = fresh () in ins "%s =l call $orto_rt_try(l %s, l %s, l %d)" addr av iv es;
      let en = enum_name_of optty in
      let (some_tag, some_lay) = ctor_layout en "Some" in
      let (none_tag, _) = ctor_layout en "None" in
      let outp = fresh () in ains "%s =l alloc8 %d" outp (size_of optty);
      let lsome = flabel "some" and lnone = flabel "none" and lj = flabel "tj" in
      ins "jnz %s, %s, %s" addr lsome lnone;
      label lsome;
        ins "storel %d, %s" some_tag outp;
        let (aty, off) = List.hd some_lay in
        let fp = fresh () in ins "%s =l add %s, %d" fp outp off;
        (if is_agg aty then ins "blit %s, %s, %d" addr fp es
         else (let lv = fresh () in ins "%s =%s %s %s" lv (qty aty) (load_ty aty) addr;
               ins "%s %s, %s" (store_ty aty) lv fp));
        ins "jmp %s" lj;
      label lnone; ins "storel %d, %s" none_tag outp; ins "jmp %s" lj;
      label lj; outp

  | TEPrint (_, exprs, _) -> List.iter (fun e -> ignore (lower e)) exprs; "0"

  (* ----- tuples ----- *)
  | TETuple (es, tty) ->
      let p = fresh () in ains "%s =l alloc8 %d" p (size_of tty);
      ignore (List.fold_left (fun off e ->
        let ev = lower e in let et = ty_of e in
        let fp = fresh () in ins "%s =l add %s, %d" fp p off;
        (if is_agg et then ins "blit %s, %s, %d" ev fp (size_of et)
         else ins "%s %s, %s" (store_ty et) ev fp);
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
      let fp = fresh () in ins "%s =l add %s, %d" fp base off;
      if is_agg comp_ty then fp
      else (let r = fresh () in ins "%s =%s %s %s" r (qty comp_ty) (load_ty comp_ty) fp; r)
  | TELetTuple (names, tty, value, body, _, _) ->
      let p = lower value in
      let tys = match tty with A.TyTuple ts -> ts | _ -> [] in
      ignore (List.fold_left2 (fun off name ct ->
        if name <> "_" then begin
          let fp = fresh () in ins "%s =l add %s, %d" fp p off;
          if is_agg ct then Hashtbl.replace locals name (Agg (fp, size_of ct))
          else begin
            let slot = fresh () in ains "%s =l alloc8 8" slot;
            let q = qty ct in
            let lv = fresh () in ins "%s =%s %s %s" lv q (load_ty ct) fp;
            ins "%s %s, %s" (store_ty ct) lv slot;
            Hashtbl.replace locals name (Scal (slot, ct))
          end
        end;
        off + size_of ct) 0 names tys);
      lower body

  | TEBorrow _ ->
      (* A borrow is an address-of; lowering it correctly needs the place in
         memory. The QBE backend doesn't do that yet — fail loudly rather than
         miscompile. The C backend supports borrows. *)
      failwith "qbe backend: borrows (&T) are not yet supported — use the C backend"

  | e ->
      let tag = match e with
        | TEMakeClosure _ -> "TEMakeClosure" | TECall _ -> "TECall-indirect"
        | TEFnRef _ -> "TEFnRef" | TEAwait _ -> "TEAwait" | TESpawn _ -> "TESpawn"
        | TEForStream _ -> "TEForStream" | TEAwaitAll _ -> "TEAwaitAll"
        | TEFloat _ -> "TEFloat" | _ -> "other"
      in
      failwith (Printf.sprintf "qbe: unimplemented %s" tag)

(* checked address of a region-handle element. If the handle var was hoisted
   by an enclosing loop, the gen-check is already done (preheader) and only a
   bounds check + address add remain; otherwise fall back to orto_rt_at. *)
and region_elem_addr (a : expr) (i : expr) (es : int) : string =
  match a with
  | TEVar (v, _) when Hashtbl.mem hoisted v ->
      let (base, len, _) = Hashtbl.find hoisted v in
      let iv = lower i in
      let blo = fresh () in ins "%s =l csltl %s, 0" blo iv;
      let bhi = fresh () in ins "%s =l csgel %s, %s" bhi iv len;
      let bad = fresh () in ins "%s =l or %s, %s" bad blo bhi;
      let lbad = flabel "hbad" and lok = flabel "hok" in
      ins "jnz %s, %s, %s" bad lbad lok;
      label lbad; ins "call $abort()"; ins "jmp %s" lok;
      label lok;
      let off = fresh () in ins "%s =l mul %s, %d" off iv es;
      let ad = fresh () in ins "%s =l add %s, %s" ad base off; ad
  | _ ->
      let av = lower a in let iv = lower i in
      let ad = fresh () in ins "%s =l call $orto_rt_at(l %s, l %s, l %d)" ad av iv es; ad

(* ===== a function ===== *)
let emit_func (f : func) : string =
  Buffer.clear buf; Buffer.clear slots_buf; tmp := 0; lbl := 0;
  Hashtbl.clear locals; loops := []; Hashtbl.clear hoisted;
  let agg_ret = is_agg f.return_ty && f.name <> "main" in
  cur_sret := None; cur_ret_size := 0;
  let prologue = Buffer.create 128 in
  (* hidden sret pointer for aggregate-returning functions *)
  let sret_sig =
    if agg_ret then begin
      cur_sret := Some "%.sret"; cur_ret_size := size_of f.return_ty;
      "l %.sret"
    end else ""
  in
  let param_sigs =
    List.mapi (fun i (name, t) ->
      let q = qty t in
      if is_agg t then begin
        (* aggregate param arrives as a pointer; bind directly *)
        Hashtbl.replace locals name (Agg (Printf.sprintf "%%a%d" i, size_of t));
        Printf.sprintf "l %%a%d" i
      end else begin
        let slot = Printf.sprintf "%%.s_%d" i in
        Buffer.add_string prologue (Printf.sprintf "\t%s =l alloc8 8\n" slot);
        Buffer.add_string prologue (Printf.sprintf "\t%s %%a%d, %s\n" (store_ty t) i slot);
        Hashtbl.replace locals name (Scal (slot, t));
        Printf.sprintf "%s %%a%d" q i
      end) f.params
  in
  (* closure body: leading env pointer, captures unpacked from it *)
  let env_sig =
    if f.takes_env then begin
      List.iter (fun (cn, ct, off) ->
        let fp = fresh () in ins "%s =l add %%.env, %d" fp off;
        if is_agg ct then Hashtbl.replace locals cn (Agg (fp, size_of ct))
        else begin
          let slot = fresh () in ains "%s =l alloc8 8" slot;
          let q = qty ct in
          let lv = fresh () in ins "%s =%s %s %s" lv q (load_ty ct) fp;
          ins "%s %s, %s" (store_ty ct) lv slot;
          Hashtbl.replace locals cn (Scal (slot, ct))
        end) (capture_layout f.captures);
      "l %.env"
    end else ""
  in
  let sig_params =
    String.concat ", " (List.filter (fun s -> s <> "") (env_sig :: sret_sig :: param_sigs))
  in
  let v = lower f.body in
  let body = Buffer.contents buf in
  let header, ret_line =
    if f.name = "main" then begin
      let q = qty f.return_ty in
      let rl =
        if q <> "w" then
          Printf.sprintf "\t%%.rs =l alloc8 8\n\t%s %s, %%.rs\n\t%%.rw =w loadw %%.rs\n\tret %%.rw\n"
            (store_ty f.return_ty) v
        else Printf.sprintf "\tret %s\n" v
      in ("w", rl)
    end else if agg_ret then
      ("", Printf.sprintf "\tblit %s, %%.sret, %d\n\tret\n" v !cur_ret_size)
    else
      (qty f.return_ty, Printf.sprintf "\tret %s\n" v)
  in
  let rtystr = if header = "" then "" else header ^ " " in
  Printf.sprintf "export function %s$%s(%s) {\n@start\n%s%s%s%s}\n"
    rtystr f.name sig_params (Buffer.contents slots_buf) (Buffer.contents prologue) body ret_line

let emit (prog : program) : string =
  Hashtbl.clear records; Hashtbl.clear enums; Hashtbl.clear strings; Hashtbl.clear wrappers;
  List.iter (fun (rd : A.record_decl) -> Hashtbl.replace records rd.A.rec_name rd) prog.records;
  List.iter (fun (td : A.type_decl) -> Hashtbl.replace enums td.A.type_name td) prog.types;
  compute_region_safe prog.funcs;
  let fns = String.concat "\n" (List.map emit_func prog.funcs) in
  (* env-adapting wrappers for plain fns used as values (TEFnRef) *)
  let wraps =
    Hashtbl.fold (fun name fnty acc ->
      let (args, ret) = match fnty with A.TyFun (a, r) -> (a, r) | _ -> ([], A.TyInt) in
      let agg = is_agg ret in
      let tparams = List.mapi (fun i t -> Printf.sprintf "%s %%a%d" (qty t) i) args in
      let tvals = String.concat ", " tparams in
      let sigp =
        String.concat ", "
          ((if agg then ["l %.env"; "l %.sret"] else ["l %.env"]) @ tparams)
      in
      let body, rsig =
        if agg then
          (Printf.sprintf "\tcall $%s(%s)\n\tret\n" name
             (String.concat ", " (("l %.sret") :: tparams)), "")
        else
          (Printf.sprintf "\t%%r =%s call $%s(%s)\n\tret %%r\n" (qty ret) name tvals,
           qty ret ^ " ")
      in
      Printf.sprintf "function %s$fnval_%s(%s) {\n@start\n%s}\n" rsig name sigp body :: acc)
      wrappers []
  in
  let fns = String.concat "\n" (wraps @ [fns]) in
  (* string literal data defs (strings interned during emit_func) *)
  let datas =
    Hashtbl.fold (fun s lab acc ->
      let bytes =
        String.to_seq s |> Seq.map (fun c -> Printf.sprintf "b %d" (Char.code c)) |> List.of_seq
      in
      let body = String.concat ", " (bytes @ ["b 0"]) in
      Printf.sprintf "data %s = { %s }" lab body :: acc) strings []
  in
  String.concat "\n" datas ^ (if datas = [] then "" else "\n") ^ fns

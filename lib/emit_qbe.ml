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
let tmp = ref 0
let lbl = ref 0
let fresh () = incr tmp; Printf.sprintf "%%.t%d" !tmp
let flabel p = incr lbl; Printf.sprintf "@.%s%d" p !lbl
let ins fmt = Printf.ksprintf (fun s -> Buffer.add_string buf ("\t" ^ s ^ "\n")) fmt
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

  | TEVar (name, _) ->
      (match Hashtbl.find locals name with
       | Agg (ptr, _) -> ptr                       (* value of an aggregate = its address *)
       | Scal (slot, ty) -> let q = qty ty in let r = fresh () in ins "%s =%s %s %s" r q (load_ty ty) slot; r)

  | TELet (name, vty, value, body, _, _) ->
      if is_agg vty then begin
        let v = lower value in
        Hashtbl.replace locals name (Agg (v, size_of vty))
      end else begin
        let v = lower value in
        let slot = fresh () in
        ins "%s =l alloc8 8" slot;
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

  | TEBinop (op, a, b, t) ->
      let va = lower a in
      let vb = lower b in
      let aq = qty (ty_of a) in
      let rq = qty t in
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
       | A.OpEq   -> ins "%s =w ceq%s %s, %s"  r aq va vb
       | A.OpNeq  -> ins "%s =w cne%s %s, %s"  r aq va vb
       | A.OpLt   -> ins "%s =w cslt%s %s, %s" r aq va vb
       | A.OpLe   -> ins "%s =w csle%s %s, %s" r aq va vb
       | A.OpGt   -> ins "%s =w csgt%s %s, %s" r aq va vb
       | A.OpGe   -> ins "%s =w csge%s %s, %s" r aq va vb);
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
      if agg then ins "%s =l alloc8 8" rslot else ins "%s =l alloc8 8" rslot;
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
      let lc = flabel "loop" and lb = flabel "body" and le = flabel "end" in
      ins "jmp %s" lc;
      label lc; let vc = lower c in ins "jnz %s, %s, %s" vc lb le;
      label lb;
      loops := (lc, le) :: !loops;
      let _ = lower body in
      loops := List.tl !loops;
      ins "jmp %s" lc;
      label le; "0"

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
      let p = fresh () in ins "%s =l alloc8 %d" p sz;
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
      let p = fresh () in ins "%s =l alloc8 %d" p sz;
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
      let rslot = fresh () in ins "%s =l alloc8 8" rslot;
      let lj = flabel "mjoin" in
      if arms = [] then (term "call $abort()")     (* absurd *)
      else begin
        let en = (match scrut_ty with A.TyApp (n,_) when is_enum n -> Some n | _ -> None) in
        let tagv =
          match en with
          | Some _ -> let t = fresh () in ins "%s =l loadl %s" t p; Some t
          | None -> None
        in
        let rec go = function
          | [] -> ()
          | (pat, _guard, body) :: rest ->
              let lnext = flabel "marm" in
              let bind_then_body () =
                let v = lower body in
                ins "%s %s, %s" (store_op rq) v rslot;
                ins "jmp %s" lj
              in
              (match pat, en, tagv with
               | A.PCtor (c, vars), Some en, Some tv ->
                   let (tag, arglay) = ctor_layout en c in
                   let cmp = fresh () in ins "%s =w ceql %s, %d" cmp tv tag;
                   let lhit = flabel "mhit" in
                   ins "jnz %s, %s, %s" cmp lhit lnext;
                   label lhit;
                   List.iteri (fun i v ->
                     if v <> "_" then begin
                       let (aty, off) = List.nth arglay i in
                       let fp = fresh () in ins "%s =l add %s, %d" fp p off;
                       if is_agg aty then Hashtbl.replace locals v (Agg (fp, size_of aty))
                       else begin
                         let slot = fresh () in ins "%s =l alloc8 8" slot;
                         let q = qty aty in
                         let lv = fresh () in ins "%s =%s %s %s" lv q (load_ty aty) fp;
                         ins "%s %s, %s" (store_ty aty) lv slot;
                         Hashtbl.replace locals v (Scal (slot, aty))
                       end
                     end) vars;
                   bind_then_body ();
                   label lnext;
                   go rest
               | A.PBind x, _, _ ->
                   (* wildcard / catch-all: bind whole scrutinee *)
                   if x <> "_" then begin
                     if is_agg scrut_ty then Hashtbl.replace locals x (Agg (p, size_of scrut_ty))
                     else begin
                       let slot = fresh () in ins "%s =l alloc8 8" slot;
                       Hashtbl.replace locals x (Scal (slot, scrut_ty));
                       ins "%s %s, %s" (store_ty scrut_ty) p slot
                     end
                   end;
                   bind_then_body ()
                   (* no lnext jump needed: catch-all is terminal *)
               | _ -> failwith "qbe: match pattern not yet supported")
        in
        go arms;
        (* fell through all arms without catch-all: abort (shouldn't happen
           for exhaustive matches) *)
        term "call $abort()"
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
        let p = fresh () in ins "%s =l alloc8 %d" p sz;
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

  (* ----- regions & handles (M3b) ----- *)
  | TERegion (n, _) | TEStackRegion (n, _) ->
      let nv = lower n in
      let p = fresh () in ins "%s =l alloc8 16" p;
      ins "call $orto_rt_region(l %s, l %s)" nv p; p
  | TEAlignedRegion (n, _a, _) ->
      let nv = lower n in
      let p = fresh () in ins "%s =l alloc8 16" p;
      ins "call $orto_rt_region(l %s, l %s)" nv p; p

  | TEHandle (r, n, init, hty) ->
      let elemt = handle_elem hty in
      let es = size_of elemt in
      let rp = lower r in
      let nv = lower n in
      let iv = lower init in
      let ip = fresh () in ins "%s =l alloc8 %d" ip (if es < 8 then 8 else es);
      if is_agg elemt then ins "blit %s, %s, %d" iv ip es
      else ins "%s %s, %s" (store_ty elemt) iv ip;
      let h = fresh () in ins "%s =l alloc8 32" h;
      ins "call $orto_rt_ref(l %s, l %s, l %d, l %s, l %s)" rp nv es ip h;
      h

  | TEHandleLit (r, elems, hty) ->
      let elemt = handle_elem hty in
      let es = size_of elemt in
      let rp = lower r in
      let n = List.length elems in
      let h = fresh () in ins "%s =l alloc8 32" h;
      ins "call $orto_rt_ref(l %s, l %d, l %d, l 0, l %s)" rp n es h;
      List.iteri (fun i el ->
        let ev = lower el in
        let addr = fresh () in ins "%s =l call $orto_rt_at(l %s, l %d, l %d)" addr h i es;
        if is_agg elemt then ins "blit %s, %s, %d" ev addr es
        else ins "%s %s, %s" (store_ty elemt) ev addr) elems;
      h

  | TEIndex (a, i, elemt) ->
      let av = lower a in let iv = lower i in let es = size_of elemt in
      let addr =
        match ty_of a with
        | A.TyPtr _ ->
            let off = fresh () in ins "%s =l mul %s, %d" off iv es;
            let ad = fresh () in ins "%s =l add %s, %s" ad av off; ad
        | _ ->
            let ad = fresh () in ins "%s =l call $orto_rt_at(l %s, l %s, l %d)" ad av iv es; ad
      in
      if is_agg elemt then addr
      else (let r = fresh () in ins "%s =%s %s %s" r (qty elemt) (load_ty elemt) addr; r)

  | TEAssignIdx (a, i, v, _) ->
      let elemt = (match ty_of a with A.TyPtr e -> e | t -> handle_elem t) in
      let av = lower a in let iv = lower i in let es = size_of elemt in
      let addr =
        match ty_of a with
        | A.TyPtr _ ->
            let off = fresh () in ins "%s =l mul %s, %d" off iv es;
            let ad = fresh () in ins "%s =l add %s, %s" ad av off; ad
        | _ ->
            let ad = fresh () in ins "%s =l call $orto_rt_at(l %s, l %s, l %d)" ad av iv es; ad
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
      let out = fresh () in ins "%s =l alloc8 32" out;
      ins "call $orto_rt_slice(l %s, l %s, l %s, l %d, l %s)" av lov hiv es out; out

  | TEReset r -> let rp = lower r in ins "call $orto_rt_reset(l %s)" rp; "0"

  | TEDrop (e, t) ->
      let v = lower e in
      (match t with
       | A.TyApp ("Region", []) -> ins "call $orto_rt_drop(l %s)" v
       | _ -> ());   (* user-linear drops: side effect only, no exit-code impact *)
      "0"

  | TEStringLit s ->
      let lab = intern_string s in
      let len = String.length s in
      let h = fresh () in ins "%s =l alloc8 32" h;
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
      let outp = fresh () in ins "%s =l alloc8 %d" outp (size_of optty);
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
      let p = fresh () in ins "%s =l alloc8 %d" p (size_of tty);
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
            let slot = fresh () in ins "%s =l alloc8 8" slot;
            let q = qty ct in
            let lv = fresh () in ins "%s =%s %s %s" lv q (load_ty ct) fp;
            ins "%s %s, %s" (store_ty ct) lv slot;
            Hashtbl.replace locals name (Scal (slot, ct))
          end
        end;
        off + size_of ct) 0 names tys);
      lower body

  | _ -> failwith "qbe: unimplemented expression (milestone in progress)"

(* ===== a function ===== *)
let emit_func (f : func) : string =
  Buffer.clear buf; tmp := 0; lbl := 0;
  Hashtbl.clear locals; loops := [];
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
  let sig_params = String.concat ", " (List.filter (fun s -> s <> "") (sret_sig :: param_sigs)) in
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
  Printf.sprintf "export function %s$%s(%s) {\n@start\n%s%s%s}\n"
    rtystr f.name sig_params (Buffer.contents prologue) body ret_line

let emit (prog : program) : string =
  Hashtbl.clear records; Hashtbl.clear enums; Hashtbl.clear strings;
  List.iter (fun (rd : A.record_decl) -> Hashtbl.replace records rd.A.rec_name rd) prog.records;
  List.iter (fun (td : A.type_decl) -> Hashtbl.replace enums td.A.type_name td) prog.types;
  let fns = String.concat "\n" (List.map emit_func prog.funcs) in
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

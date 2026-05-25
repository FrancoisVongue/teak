(* QBE backend — alternative to the C emitter (emit.ml).
   Front-end (parse→resolve→lift→check→mono) shared; this lowers the
   typed, monomorphized program to QBE IL (.ssa).
     M1: functions, int/bool literals, ret.
     M2: arithmetic, comparisons, let (stack slots), if/while, return/break/continue.
   Unlowered nodes raise failwith so tests only hit implemented constructs.
   All locals live in stack slots (alloc8) — correct, no SSA/phi; QBE can
   promote them. *)

open Check.T
module A = Ast

(* ---------- QBE base type of an orto type ---------- *)
let qty (t : A.ty) : string =
  match t with
  | A.TyInt -> "l"
  | A.TyBool -> "w"
  | A.TyApp (("byte"|"u8"|"i8"|"u16"|"i16"|"u32"|"i32"), []) -> "w"
  | A.TyApp (("u64"|"i64"|"u128"|"i128"), []) -> "l"
  | A.TyApp ("f32", []) -> "s"
  | A.TyApp (("f64"|"float"), []) -> "d"
  | A.TyPtr _ -> "l"
  | A.TyTuple [] -> "w"
  | _ -> "l"

let store_op = function "w"->"storew" | "d"->"stored" | "s"->"stores" | _->"storel"
let load_op  = function "w"->"loadw"  | "d"->"loadd"  | "s"->"loads"  | _->"loadl"

(* ---------- per-function emit state ---------- *)
let buf = Buffer.create 1024
let tmp = ref 0
let lbl = ref 0
let fresh () = incr tmp; Printf.sprintf "%%.t%d" !tmp
let flabel p = incr lbl; Printf.sprintf "@.%s%d" p !lbl
let ins fmt = Printf.ksprintf (fun s -> Buffer.add_string buf ("\t" ^ s ^ "\n")) fmt
let label l = Buffer.add_string buf (l ^ "\n")
(* terminator: emit it, then open a fresh (possibly dead) block so any
   following instructions remain valid QBE. *)
let term fmt =
  Printf.ksprintf (fun s ->
    Buffer.add_string buf ("\t" ^ s ^ "\n");
    label (flabel "dead")) fmt

let locals : (string, string * string) Hashtbl.t = Hashtbl.create 16
let loops : (string * string) list ref = ref []

(* ---------- type of an expression node (only what M2 lowers) ---------- *)
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
  | TEReturn _ -> A.TyInt
  | _ -> A.TyInt

(* ---------- numeric conversion: produce v as dst qbe type ---------- *)
let convert (sq : string) (dq : string) (v : string) : string =
  if sq = dq then v
  else begin
    let r = fresh () in
    (match sq, dq with
     | "w", "l"            -> ins "%s =l extsw %s" r v        (* widen (bit31 of our ws is 0) *)
     | "l", "w"            -> ins "%s =w copy %s" r v         (* truncate: low word *)
     | ("w"|"l"), "d"      -> ins "%s =d s%stof %s" r sq v    (* swtof / sltof *)
     | ("w"|"l"), "s"      -> ins "%s =s s%stof %s" r sq v
     | "d", ("w"|"l")      -> ins "%s =%s dtosi %s" r dq v
     | "s", ("w"|"l")      -> ins "%s =%s stosi %s" r dq v
     | "s", "d"            -> ins "%s =d exts %s" r v
     | "d", "s"            -> ins "%s =s truncd %s" r v
     | _                   -> ins "%s =%s copy %s" r dq v);
    r
  end

(* ---------- lower an expression to a QBE operand ---------- *)
let rec lower (e : expr) : string =
  match e with
  | TEInt n  -> Int64.to_string n
  | TEBool b -> if b then "1" else "0"

  | TEVar (name, _) ->
      let (slot, q) = Hashtbl.find locals name in
      let r = fresh () in ins "%s =%s %s %s" r q (load_op q) slot; r

  | TELet (name, vty, value, body, _, _) ->
      let v = lower value in
      let q = qty vty in
      let slot = fresh () in
      ins "%s =l alloc8 8" slot;
      ins "%s %s, %s" (store_op q) v slot;
      Hashtbl.replace locals name (slot, q);
      lower body

  | TEAssign (name, value, _) ->
      let v = lower value in
      let (slot, q) = Hashtbl.find locals name in
      ins "%s %s, %s" (store_op q) v slot;
      "0"

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
       | A.OpBAnd -> ins "%s =%s and %s, %s"  r rq va vb
       | A.OpBOr  -> ins "%s =%s or %s, %s"   r rq va vb
       | A.OpBXor -> ins "%s =%s xor %s, %s"  r rq va vb
       | A.OpShl  -> ins "%s =%s shl %s, %s"  r rq va vb
       | A.OpShr  -> ins "%s =%s sar %s, %s"  r rq va vb
       | A.OpAnd  -> ins "%s =%s and %s, %s"  r rq va vb   (* bools 0/1; non-short-circuit *)
       | A.OpOr   -> ins "%s =%s or %s, %s"   r rq va vb
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
      let q = qty t in
      let rslot = fresh () in ins "%s =l alloc8 8" rslot;
      let lt = flabel "then" and le = flabel "else" and lj = flabel "join" in
      let vc = lower c in
      ins "jnz %s, %s, %s" vc lt le;
      label lt;
      let vt = lower th in ins "%s %s, %s" (store_op q) vt rslot; ins "jmp %s" lj;
      label le;
      let ve = lower el in ins "%s %s, %s" (store_op q) ve rslot; ins "jmp %s" lj;
      label lj;
      let r = fresh () in ins "%s =%s %s %s" r q (load_op q) rslot; r

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
  | TEReturn (v, _) -> let vv = lower v in term "ret %s" vv; "0"

  | TEToInt e          -> convert (qty (ty_of e)) "l" (lower e)
  | TEToIntFromFloat e -> convert (qty (ty_of e)) "l" (lower e)
  | TEToFloat e        -> convert (qty (ty_of e)) "d" (lower e)
  | TEToByte e ->
      let v = lower e in let r = fresh () in ins "%s =w and %s, 255" r v; r
  | TECast (target, e) ->
      convert (qty (ty_of e)) (qty (A.TyApp (target, []))) (lower e)

  (* Direct call (callee is a function/extern name after mono). First-class
     function pointers / closures are M5. *)
  | TECall (TEFnRef (name, _, _), args, ret) ->
      let avs = List.map (fun a -> (qty (ty_of a), lower a)) args in
      let argstr =
        String.concat ", "
          (List.map (fun (q, v) -> Printf.sprintf "%s %s" q v) avs)
      in
      let rq = qty ret in
      let r = fresh () in
      ins "%s =%s call $%s(%s)" r rq name argstr;
      r

  | _ -> failwith "qbe: unimplemented expression (milestone in progress)"

(* ---------- a function ---------- *)
let emit_func (f : func) : string =
  Buffer.clear buf; tmp := 0; lbl := 0;
  Hashtbl.clear locals; loops := [];
  (* signature params are positional %aN; copy each into a stack slot
     keyed by its orto name so reads/writes are uniform. *)
  let sig_params =
    String.concat ", "
      (List.mapi (fun i (_, t) -> Printf.sprintf "%s %%a%d" (qty t) i) f.params)
  in
  let prologue = Buffer.create 128 in
  List.iteri (fun i (name, t) ->
    let q = qty t in
    let slot = Printf.sprintf "%%.s_%d" i in
    Buffer.add_string prologue (Printf.sprintf "\t%s =l alloc8 8\n" slot);
    Buffer.add_string prologue (Printf.sprintf "\t%s %%a%d, %s\n" (store_op q) i slot);
    Hashtbl.replace locals name (slot, q)) f.params;
  let v = lower f.body in
  let body = Buffer.contents buf in
  let rty = if f.name = "main" then "w" else qty f.return_ty in
  (* main must return a C int (w); truncate via memory if body is l. *)
  let ret_line =
    if f.name = "main" && qty f.return_ty <> "w" then
      let q = qty f.return_ty in
      Printf.sprintf "\t%%.rs =l alloc8 8\n\t%s %s, %%.rs\n\t%%.rw =w loadw %%.rs\n\tret %%.rw\n"
        (store_op q) v
    else Printf.sprintf "\tret %s\n" v
  in
  Printf.sprintf "export function %s $%s(%s) {\n@start\n%s%s%s}\n"
    rty f.name sig_params (Buffer.contents prologue) body ret_line

let emit (prog : program) : string =
  String.concat "\n" (List.map emit_func prog.funcs)

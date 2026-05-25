(* QBE backend — alternative to the C emitter (emit.ml).

   The front-end (parse → resolve → lift → check → mono) is shared; this
   module lowers the typed, monomorphized program to QBE IL (.ssa), which
   `qbe` turns into assembly. Built milestone by milestone:
     M1: functions, int literals, ret.
     M2: arithmetic, let (stack slots), if/while, comparisons.
     M3: calls + C runtime (regions/handles via runtime.c).
     M4: structs/enums/match.
   Anything not yet lowered raises a clear failwith so tests only ever
   exercise implemented nodes. *)

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
  | A.TyTuple [] -> "w"          (* unit — represented as a word 0 *)
  | _ -> "l"

(* ---------- per-function emit state ---------- *)
let buf = Buffer.create 1024
let tmp = ref 0
let fresh () = incr tmp; Printf.sprintf "%%.t%d" !tmp
let ins fmt = Printf.ksprintf (fun s -> Buffer.add_string buf ("\t" ^ s ^ "\n")) fmt

(* ---------- lower an expression to a QBE operand ---------- *)
let lower (e : expr) : string =
  match e with
  | TEInt n  -> Int64.to_string n
  | TEBool b -> if b then "1" else "0"
  | _ -> failwith "qbe: unimplemented expression (milestone in progress)"

(* ---------- a function ---------- *)
let emit_func (f : func) : string =
  Buffer.clear buf; tmp := 0;
  let v = lower f.body in
  let body = Buffer.contents buf in
  let rty = if f.name = "main" then "w" else qty f.return_ty in
  let params =
    String.concat ", "
      (List.map (fun (n, t) -> Printf.sprintf "%s %%%s" (qty t) n) f.params)
  in
  Printf.sprintf "export function %s $%s(%s) {\n@start\n%s\tret %s\n}\n"
    rty f.name params body v

let emit (prog : program) : string =
  String.concat "\n" (List.map emit_func prog.funcs)

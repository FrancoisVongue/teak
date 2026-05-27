(* Driver.

   Reads an .orto entry file, follows its `use` statements to load
   any referenced modules from the same directory, runs the full
   pipeline (parse → resolve → check → mono → emit), writes a .c file.

   A `use` path maps to a file by joining its segments: `use a::b::c;`
   loads `a/b/c.orto`, searched in the entry dir first then the library
   root (ORTO_ROOT, default "."). The loaded module's namespace IS that
   path, so the standard library under std/ is addressed as `std::vec`,
   `std::map`, etc. Cycles are handled by memoization.

   Usage: orto INPUT.orto [-o OUTPUT.c] *)

let read_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let write_file path content =
  let oc = open_out path in
  output_string oc content;
  close_out oc

let default_output input =
  if Filename.check_suffix input ".orto"
  then Filename.chop_suffix input ".orto" ^ ".c"
  else input ^ ".c"

(* The entry file's namespace is its bare basename (a single segment);
   it is given directly, not `use`d. *)
let module_name_of_path path =
  [ Filename.basename path |> Filename.chop_extension ]

(* A `use` path maps directly to a file: a::b::c -> a/b/c.orto, and the
   loaded module's namespace IS that path. Single-segment paths (e.g.
   `use sys`) stay project-local; `std::vec` lives under std/. *)
let path_key (segs : string list) = String.concat "::" segs
let path_to_file (segs : string list) = String.concat "/" segs ^ ".orto"

(* Memoized loader. modules_loaded maps a module's path-key to its
   (path, parsed program); load_order keeps insertion order. *)
let modules_loaded : (string, string list * Orto.Ast.program) Hashtbl.t =
  Hashtbl.create 8
let load_order : string list ref = ref []

(* Recursively walk all top-level decls including inside namespace
   blocks, returning every `use` path (= module to load). *)
let rec collect_use_files decls =
  List.concat_map (function
    | Orto.Ast.TopUse u ->
        (match u.Orto.Ast.use_module with [] -> [] | segs -> [segs])
    | Orto.Ast.TopNamespace (_, inner) -> collect_use_files inner
    | _ -> []) decls

(* Search roots for a `use`d module path: the entry program's directory
   first (so a project can shadow / provide its own modules), then the
   library root (ORTO_ROOT, default "." — std/ lives directly under it,
   so `use std::vec` resolves to <root>/std/vec.orto). *)
let lib_root =
  try Sys.getenv "ORTO_ROOT" with Not_found -> "."

let find_module entry_dir segs =
  let rel = path_to_file segs in
  let candidates =
    [ Filename.concat entry_dir rel; Filename.concat lib_root rel ]
  in
  let rec first = function
    | [] -> None
    | p :: rest -> if Sys.file_exists p then Some p else first rest
  in
  first candidates

let rec load_module entry_dir segs visiting =
  (* Memoization handles mutual references — A imports B imports A is
     fine, both end up loaded once. `visiting` is kept for future
     debugging but no longer used to reject cycles. *)
  let _ = visiting in
  let key = path_key segs in
  if Hashtbl.mem modules_loaded key then ()
  else begin
    let path =
      match find_module entry_dir segs with
      | Some p -> p
      | None ->
          failwith (Printf.sprintf
            "module %S referenced via `use`, but %s not found \
             (searched %s and %s)"
            key (path_to_file segs) entry_dir lib_root)
    in
    let src = read_file path in
    let toks = Orto.Lexer.lex src in
    let ast = Orto.Parser.parse toks in
    Hashtbl.add modules_loaded key (segs, ast);
    load_order := key :: !load_order;
    List.iter (fun child ->
      load_module entry_dir child (key :: visiting))
      (collect_use_files ast)
  end

(* CLI is intentionally tiny — flag parsing in OCaml's Arg is heavy
   for our needs. Walk argv once and pick out --slots / --cores. *)
let parse_args () =
  let input    = ref None in
  let output   = ref None in
  let slots    = ref 1024 in
  let cores    = ref 1 in
  let ring     = ref 64 in
  let test     = ref false in
  let backend  = ref "c" in
  let argv = Sys.argv in
  let n = Array.length argv in
  let i = ref 1 in
  let usage () =
    prerr_endline
      "usage: orto INPUT.orto [-o OUTPUT.c] [--slots N] [--cores N] [--ring-entries N] [--test]";
    exit 2
  in
  while !i < n do
    (match argv.(!i) with
     | "-o" ->
         if !i + 1 >= n then usage ();
         output := Some argv.(!i + 1);
         i := !i + 2
     | "--slots" ->
         if !i + 1 >= n then usage ();
         slots := int_of_string argv.(!i + 1);
         i := !i + 2
     | "--cores" ->
         if !i + 1 >= n then usage ();
         cores := int_of_string argv.(!i + 1);
         i := !i + 2
     | "--ring-entries" ->
         if !i + 1 >= n then usage ();
         ring := int_of_string argv.(!i + 1);
         i := !i + 2
     | "--test" ->
         test := true;
         incr i
     | "--backend" ->
         if !i + 1 >= n then usage ();
         backend := argv.(!i + 1);
         i := !i + 2
     | s when !input = None ->
         input := Some s;
         incr i
     | _ -> usage ())
  done;
  let input = match !input with Some s -> s | None -> usage () in
  let output = match !output with Some s -> s | None -> default_output input in
  if !slots <= 0 then begin prerr_endline "--slots must be > 0"; exit 2 end;
  if !cores <= 0 then begin prerr_endline "--cores must be > 0"; exit 2 end;
  if !ring  <= 0 then begin prerr_endline "--ring-entries must be > 0"; exit 2 end;
  (input, output, !slots, !cores, !ring, !test, !backend)

let () =
  let (input, output, slots, cores, ring_entries, test_mode, backend) = parse_args () in
  let entry_dir = Filename.dirname input in
  let entry_module = module_name_of_path input in
  try
    let src = read_file input in
    let toks = Orto.Lexer.lex src in
    let ast = Orto.Parser.parse toks in
    let entry_key = path_key entry_module in
    Hashtbl.add modules_loaded entry_key (entry_module, ast);
    load_order := entry_key :: !load_order;
    List.iter (fun child ->
      load_module entry_dir child [entry_key])
      (collect_use_files ast);
    (* Preserve insertion order (entry first, dependencies after);
       resolve.ml doesn't care about order, only about completeness. *)
    let modules =
      List.rev_map (fun key -> Hashtbl.find modules_loaded key)
        !load_order
    in
    let merged = Orto.Resolve.resolve modules in
    let merged = Orto.Lift.lift merged in
    let typed_ast = Orto.Check.check merged in
    let mono = Orto.Mono.monomorphize typed_ast in
    let out =
      match backend with
      | "qbe" -> Orto.Emit_qbe.emit mono
      | "c" -> Orto.Emit.emit ~slots ~cores ~ring_entries ~test_mode mono
      | b -> failwith (Printf.sprintf "unknown --backend %S (use c or qbe)" b)
    in
    write_file output out;
    Printf.printf "wrote %s\n" output
  with
  | Orto.Lexer.Lex_error (msg, pos) ->
      Printf.eprintf "lex error at byte %d: %s\n" pos msg;
      exit 1
  | Orto.Parser.Parse_error msg ->
      Printf.eprintf "parse error: %s\n" msg;
      exit 1
  | Orto.Resolve.Resolve_error msg ->
      Printf.eprintf "module error: %s\n" msg;
      exit 1
  | Orto.Lift.Lift_error msg ->
      Printf.eprintf "lambda error: %s\n" msg;
      exit 1
  | Orto.Check.Type_error msg ->
      Printf.eprintf "type error: %s\n" msg;
      exit 1
  | Failure msg ->
      Printf.eprintf "%s\n" msg;
      exit 1

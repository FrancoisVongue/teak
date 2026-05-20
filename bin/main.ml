(* Driver.

   Reads an .orto entry file, follows its `use` statements to load
   any referenced modules from the same directory, runs the full
   pipeline (parse → resolve → check → mono → emit), writes a .c file.

   A module's name is the basename of its .orto file. `use foo::bar;`
   loads `<entry_dir>/foo.orto`. Cycles are rejected.

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

let module_name_of_path path =
  Filename.basename path |> Filename.chop_extension

(* Memoized loader. modules_loaded keeps insertion order; visiting
   tracks the current DFS path for cycle detection. *)
let modules_loaded : (string, Orto.Ast.program) Hashtbl.t = Hashtbl.create 8
let load_order : string list ref = ref []

(* Recursively walk all top-level decls including inside namespace
   blocks, returning every `use` path's first component (= file name
   to load). *)
let rec collect_use_files decls =
  List.concat_map (function
    | Orto.Ast.TopUse u ->
        (match u.Orto.Ast.use_module with
         | first :: _ -> [first]
         | [] -> [])
    | Orto.Ast.TopNamespace (_, inner) -> collect_use_files inner
    | _ -> []) decls

let rec load_module entry_dir mod_name visiting =
  (* Memoization handles mutual references — A imports B imports A is
     fine, both end up loaded once. `visiting` is kept for future
     debugging but no longer used to reject cycles. *)
  let _ = visiting in
  if Hashtbl.mem modules_loaded mod_name then ()
  else begin
    let path = Filename.concat entry_dir (mod_name ^ ".orto") in
    let src =
      try read_file path
      with Sys_error _ ->
        failwith (Printf.sprintf
          "module %S referenced via `use`, but %s not found"
          mod_name path)
    in
    let toks = Orto.Lexer.lex src in
    let ast = Orto.Parser.parse toks in
    Hashtbl.add modules_loaded mod_name ast;
    load_order := mod_name :: !load_order;
    List.iter (fun child ->
      load_module entry_dir child (mod_name :: visiting))
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
  (input, output, !slots, !cores, !ring, !test)

let () =
  let (input, output, slots, cores, ring_entries, test_mode) = parse_args () in
  let entry_dir = Filename.dirname input in
  let entry_module = module_name_of_path input in
  try
    let src = read_file input in
    let toks = Orto.Lexer.lex src in
    let ast = Orto.Parser.parse toks in
    Hashtbl.add modules_loaded entry_module ast;
    load_order := entry_module :: !load_order;
    List.iter (fun child ->
      load_module entry_dir child [entry_module])
      (collect_use_files ast);
    (* Preserve insertion order (entry first, dependencies after);
       resolve.ml doesn't care about order, only about completeness. *)
    let modules =
      List.rev_map (fun mn -> (mn, Hashtbl.find modules_loaded mn))
        !load_order
    in
    let merged = Orto.Resolve.resolve modules in
    let merged = Orto.Lift.lift merged in
    let typed_ast = Orto.Check.check merged in
    let mono = Orto.Mono.monomorphize typed_ast in
    let c = Orto.Emit.emit ~slots ~cores ~ring_entries ~test_mode mono in
    write_file output c;
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

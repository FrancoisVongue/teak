(* Driver: reads an .orto source file, runs the pipeline,
   writes a .c file with the result.

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

let () =
  let args = Array.to_list Sys.argv in
  let (input, output) =
    match args with
    | [_; inp]               -> (inp, default_output inp)
    | [_; inp; "-o"; outp]   -> (inp, outp)
    | _ ->
        prerr_endline "usage: orto INPUT.orto [-o OUTPUT.c]";
        exit 2
  in
  let src = read_file input in
  try
    let toks = Orto.Lexer.lex src in
    let ast  = Orto.Parser.parse toks in
    let typed_ast = Orto.Check.check ast in
    let mono = Orto.Mono.monomorphize typed_ast in
    let c = Orto.Emit.emit mono in
    write_file output c;
    Printf.printf "wrote %s\n" output
  with
  | Orto.Lexer.Lex_error (msg, pos) ->
      Printf.eprintf "lex error at byte %d: %s\n" pos msg;
      exit 1
  | Orto.Parser.Parse_error msg ->
      Printf.eprintf "parse error: %s\n" msg;
      exit 1
  | Orto.Check.Type_error msg ->
      Printf.eprintf "type error: %s\n" msg;
      exit 1

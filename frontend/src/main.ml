open Lexing

let compile_file filename outputname =
  let filebuf = Batteries.File.with_file_in filename Batteries.IO.read_all in
  let lexedbuf = Lexing.from_string filebuf in
  try
    let res = (Parser.entry Lexer.token lexedbuf) in
    (*Semantics.semantics_analysis res;*)
    Bytecode.compile_to outputname res
  with
  | Lexer.Error msg ->
     Printf.eprintf "%s%!" msg;
     exit (-1)
  | Parser.Error ->
     let start_pos = Lexing.lexeme_start_p lexedbuf in
     let bpos = start_pos.pos_cnum - start_pos.pos_bol in
     Printf.eprintf "At offset %d / line: %d -- syntax error.\n%!" bpos (start_pos.pos_lnum);
     exit (-2)

let () =
  let binname = "ytsc" in
  let input_file = ref None in
  let output_file = ref "out.ytx" in

  let usagemsg = Printf.sprintf "Usage: %s [filename] <options>\n" binname in
  let speclist = [
    ("-o", Arg.Set_string output_file, " specify output file name")
  ] in
  Arg.parse speclist (fun s -> (input_file := Some s)) usagemsg;

  match !input_file with
    Some filename -> compile_file filename !output_file
  | None -> begin
            Printf.eprintf "filename is not given";
            exit (-2)
          end

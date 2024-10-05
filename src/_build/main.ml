open Lexing
open Parser
open Lexer
open Codegen
open Expr

let () =
  if Array.length Sys.argv < 3 then (
    Printf.eprintf "Usage: %s <input file> <output LLVM file>\n" Sys.argv.(0);
    exit 1
  );

  let filename = Sys.argv.(1) in
  let filename_llvm = Sys.argv.(2) in

  try
    let in_channel = open_in filename in
    let lexbuf = Lexing.from_channel in_channel in

    let program_block = Parser.program Lexer.token lexbuf in
    close_in in_channel;

    Codegen.codegen_main program_block filename_llvm;

    Printf.printf "Successfully generated LLVM IR in %s\n" filename_llvm
  with
  | Lexer.Eof -> Printf.eprintf "Error: Unexpected end of file\n"; exit 1
  | Parsing.Parse_error -> Printf.eprintf "Error: Parsing error in file %s\n" filename; exit 1
  | Codegen.Error msg -> Printf.eprintf "Codegen error: %s\n" msg; exit 1


open Format
open Helping_types
open Tony_symbol
open Identifier
open Tony_ActRec
open Tony_ast
open PrintAst
open Tony_sem


let main =
  let lexbuf = Lexing.from_channel stdin in
    try
      let asts = Tony_parser.program Tony_lexer.lexer lexbuf in
        (*print_ast asts;
        run asts;
        print_act_rec (); *)
        sem asts;
    	  exit 0
    with
	  | Parsing.Parse_error ->
    	  (Printf.eprintf "syntax error in line %d\n" lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum;
    	   exit 1)
    | Match_failure (s,n1,n2) ->
        (Printf.eprintf "match error(%s, %d, %d)\n" s n1 n2; (*print_act_rec();*)
         exit 1)
    | TypeErr x ->
        (Printf.eprintf "semantic error %d\n" x;
         exit 1)

open Helping_types
open Tony_ActRec
open Tony_ast


let main =
  let lexbuf = Lexing.from_channel stdin in
  	try
     let asts = Tony_parser.program Tony_lexer.lexer lexbuf in
      run asts;
	  print_act_rec ();
    	exit 0
  	with 
	|Parsing.Parse_error ->
    	(Printf.eprintf "syntax error in line %d\n" lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum;
    	exit 1)
	

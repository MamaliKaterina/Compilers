let main =
  let lexbuf = Lexing.from_channel stdin in
  try
    Tony_parser.program Tony_lexer.lexer lexbuf;
    exit 0
  with Parsing.Parse_error ->
    Printf.eprintf "syntax error\n";
    exit 1

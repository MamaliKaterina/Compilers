(* from Papaspyrou, but might need some changes with some names, i.e. Lexer, Parser etc *)
let main =
  let lexbuf = Lexing.from_channel stdin in
  try
    Parser.program Lexer.lexer lexbuf;
    exit 0
  with Parsing.Parse_error ->
    Printf.eprintf "syntax error\n";
    exit 1

{
	open Lexing
	open Tony_parser

	let char_for_backslash = function
	    'n'  -> '\n'
    | 't'  -> '\t'
		| 'r'  -> '\r'
		| '0'  -> '\000'
	  | '\\' -> '\092'
	  | '\'' -> '\039'
		| '"'  -> '\"'
	  |  c   -> c

	let hex_digit_value d =
		let d = Char.code d in
		  if d >= 97 then d - 87 else
		    if d >= 65 then d - 55 else d - 48

	let char_for_hexadecimal_code d u =
    Char.chr (16 * (hex_digit_value d) + (hex_digit_value u))

  exception Unterminated_string
  }

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let empty = [' ' '\t' '\r']
let escape_seq = '\\' (['n' 't' 'r' '0' '\\' '\'' '\"'] | 'x'['0'-'9' 'a'-'f']['0'-'9' 'a'-'f'])
let backslash_escapes = ['\\' '\'' '"' 'n' 't' 'r' '0']

rule lexer = parse
	| "<*"		{comments 0 lexbuf}
	| "and" 	{T_and}
	| "bool" 	{T_bool}
	| "char" 	{T_char}
	| "decl" 	{T_decl}
	| "def" 	{T_def}
	| "else"	{T_else}
	| "elsif"	{T_elsif}
	| "end"		{T_end}
	| "exit"	{T_exit}
	| "false"	{T_false}
	| "for"		{T_for}
	| "head"	{T_head}
	| "if"		{T_if}
	| "int"		{T_int}
	| "list"	{T_list}
	| "mod"		{T_mod}
	| "new"		{T_new}
	| "nil"		{T_nil}
	| "nil?"	{T_is_nil}
	| "not"		{T_not}
	| "or"		{T_or}
	| "ref"		{T_ref}
	| "return"	{T_return}
	| "skip"	{T_skip}
	| "tail"	{T_tail}
	| "true"	{T_true}

	| letter (letter | digit | ['_' '?'])* {T_var (lexeme lexbuf)}
	| digit+	{T_int_const (int_of_string (lexeme lexbuf))}

	| "'" [^ '\\'] "'" {T_char_const (lexeme_char lexbuf 1)}
  | "'" '\\' backslash_escapes "'" {T_char_const (char_for_backslash (lexeme_char lexbuf 2))}
	| "'" '\\' 'x' (['0'-'9' 'a'-'f'] as d) (['0'-'9' 'a'-'f'] as u) "'" {T_char_const (char_for_hexadecimal_code d u)}

	| '"' {let buffer = Buffer.create 100 in T_string_const (string buffer lexbuf)}

	| '=' {T_eq}
	| '+'	{T_plus}
	| '-'	{T_minus}
	| '*'	{T_times}
	| '/'	{T_div}
	| '#'	{T_cons}
	| "<>"	{T_dif}
	| '<'	{T_less}
	| '>'	{T_greater}
	| "<="	{T_less_eq}
	| ">="	{T_greater_eq}

	| '('	{T_lbracket}
	| ')'	{T_rbracket}
	| '['	{T_lsqbracket}
	| ']'	{T_rsqbracket}
	| ';'	{T_semicolon}
	| ':'	{T_colon}
	| ','	{T_comma}
	| ":="	{T_assign}

	| '\n'	{new_line lexbuf; lexer lexbuf}
	| empty+	{lexer lexbuf}
	| "%" [^ '\n']* "\n"	{new_line lexbuf; lexer lexbuf}

	| eof	{T_eof}

	| _ as chr	{ Printf.eprintf "invalid character: '%c' (ascii: %d)\n"
					chr (Char.code chr);
				  lexer lexbuf}


and comments level = parse
	| "*>" { if level = 0 then lexer lexbuf else comments (level-1) lexbuf }
	| "<*" { comments (level+1) lexbuf }
	| '\n' { new_line lexbuf; comments level lexbuf }
	| _ 	 { comments level lexbuf }
	| eof	 { print_endline "comments are not closed"; T_eof }


and string buf = parse
  | '"' { Buffer.contents buf }
	| '\\' (backslash_escapes as c) { (Buffer.add_char buf (char_for_backslash c)); string buf lexbuf }
	| '\\' 'x' (['0'-'9' 'a'-'f'] as d) (['0'-'9' 'a'-'f'] as u) { (Buffer.add_char buf (char_for_hexadecimal_code d u)); string buf lexbuf }
	| '\\' (_ as c) { Buffer.add_char buf '\\'; Buffer.add_char buf c; string buf lexbuf }
	| eof { raise Unterminated_string }
	| _ as c { Buffer.add_char buf c; string buf lexbuf }

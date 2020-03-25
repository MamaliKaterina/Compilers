{
type token =
	| T_eof | T_and | T_bool | T_char | T_decl | T_def | T_else | T_elsif | T_end | T_exit | T_false | T_for | T_head | T_if | T_int | T_list | T_mod | T_new | T_nil | T_is_nil | T_not | T_or | T_ref | T_return | T_skip | T_tail | T_true
	| T_var 
	| T_int_const 
	| T_char_const 
	| T_string_const 
	| T_eq | T_minus | T_plus | T_times | T_div | T_cons | T_dif | T_less | T_greater | T_less_eq | T_greater_eq 
	| T_lbracket | T_rbracket | T_assign | T_lsqbracket | T_rsqbracket | T_colon | T_semicolon | T_comma
}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let empty = [' ' '\t' '\n' '\r']
let escape_seq = ['\n' '\t' '\r' '\0' '\\' '\'' '\"' '\x00'-'\xff']	(* the last one is probably wrong *)

rule lexer = parse
	  "and" 	{T_and}
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
	| "is_nil"	{T_is_nil}
	| "not"		{T_not}
	| "or"		{T_or}
	| "ref"		{T_ref}
	| "return"	{T_return}
	| "skip"	{T_skip}
	| "tail"	{T_tail}
	| "true"	{T_true}

	| letter [letter digit '_' '?']*	{T_var}
	| digit+	{T_int_const}
	| "'" [^ '\\' '\'' '\"']+ "'"	{T_char_const}	(*??? Maybe we could return the ascii code of the character read, say: "'" [_] as id "'" {id}??? *)
	| "\"" ([^ '\"'] | \\_)* "\""	{T_string_const}	(*???*)

	| '=' 	{T_eq}
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

	| empty+	{lexer lexbuf}
	| "%" [^ '\n']* "\n"	{lexer lexbuf}
	| "<*" ([^ '*']+ | '\*'+ [^*>])* '\*'+ "*>"	{lexer lexbuf}	(*wrong, here we recognise and maybe count??? the lines of comments*)

	| eof	{T_eof}
	| _ as chr	{ Printf.eprintf "invalid character: '%c' (ascii: %d)"
					chr (Char.code chr);
				  lexer lexbuf }

{
let string_of_token token =
  match token with
	| T_eof		-> "T_eof"
	| T_and		-> "T_and"
	| T_bool	-> "T_bool"
	| T_char	-> "T_char"
	| T_decl	-> "T_decl"
	| T_def		-> "T_def"
	| T_else	-> "T_else"
	| T_elsif	-> "T_elsif"
	| T_end		-> "T_end"
	| T_exit	-> "T_exit"
	| T_false	-> "T_false"
	| T_for		-> "T_for"
	| T_head	-> "T_head"
	| T_if		-> "T_if"
	| T_int		-> "T_int"
	| T_list	-> "T_list"
	| T_mod		-> "T_mod"
	| T_new		-> "T_new"
	| T_nil		-> "T_nil"
	| T_is_nil	-> "T_is_nil"
	| T_not		-> "T_not"
	| T_or		-> "T_or"
	| T_ref		-> "T_ref"
	| T_return	-> "T_return"
	| T_skip	-> "T_skip"
	| T_tail	-> "T_tail"
	| T_true	-> "T_true"
	| T_var		-> "T_var"
	| T_int_const	-> "T_int_const"
	| T_char_const	-> "T_char_const"
	| T_string_const	-> "T_string_const"
	| T_eq		-> "T_eq"
	| T_plus	-> "T_plus"
	| T_minus	-> "T_minus"
	| T_times	-> "T_times"
	| T_div		-> "T_div"
	| T_cons	-> "T_cons"
	| T_dif		-> "T_dif"
	| T_less	-> "T_less"
	| T_greater	-> "T_greater"
	| T_less_eq	-> "T_less_eq"
	| T_greater_eq	-> "T_greater_eq"
	| T_lbracket	-> "T_lbracket"
	| T_rbracket	-> "T_rbracket"
	| T_lsqbracket	-> "T_lsqbracket"
	| T_rsqbracket	-> "T_rsqbracket"
	| T_assign	-> "T_assign"
	| T_colon	-> "T_colon"
	| T_semicolon	-> "T_semicolon"
	| T_comma	-> "T_comma"

let main =
	let lexbuf = Lexing.from_channel stdin in
	let rec loop () =
		let token = lexer lexbuf in
		Printf.printf "token=%s, lexeme=\"%s\"\n" (string_of_token token) (Lexing.lexeme lexbuf);
		if token <> T_eof then loop () in
	loop ()
}

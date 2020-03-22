{
type token =
	| T_eof | T_and | T_bool | T_char | T_decl | T_def | T_else | T_elsif | T_end | T_exit | T_false | T_for | T_head | T_if | T_int | T_list | T_mod | T_new | T_nil | T_nil? | T_not | T_or | T_ref | T_return | T_skip | T_tail | T_true
	| T_var 
	| T_int_const 
	| T_char_const 
	| T_string_const 
	| T_eq | T_minus | T_plus | T_times | T_div | T_cons | T_dif | T_less | T_bigger | T_less_eq | T_bigger_eq 
	| T_lbracket | T_rbracket | T_define | T_lsqbracket | T_rsqbracket | T_colon | T_semicolon
}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']

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
	| "nil?"	{T_nil?}
	| "not"		{T_not}
	| "or"		{T_or}
	| "ref"		{T_ref}
	| "return"	{T_return}
	| "skip"	{T_skip}
	| "tail"	{T_tail}
	| "true"	{T_true}

	| letter [letter digit '_' '?']*	{T_var}
	| digit+	{T_int_const}
	| 	{T_char_const}
	| 	{T_string_const}

	| '=' 	{T_eq}
	| '+'	{T_plus}
	| '-'	{T_minus}
	| '*'	{T_times}
	| '/'	{T_div}
	| '#'	{T_cons}
	| "<>"	{T_dif}
	| '<'	{T_less}
	| '>'	{T_bigger}
	| "<="	{T_less_eq}
	| ">="	{T_bigger_eq}

	| '('	{T_lbracket}
	| ')'	{T_rbracket}
	| '['	{T_lsqbracket}
	| ']'	{T_rsqbracket}
	| ';'	{T_semicolon}
	| ':'	{T_colon}
	| ":="	{T_define}

	| eof 	{T_eof}
	|  _ as chr     { Printf.eprintf "invalid character: '%c' (ascii: %d)"
                      chr (Char.code chr);
                    lexer lexbuf }


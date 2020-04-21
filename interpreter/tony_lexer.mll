{
	open Lexing
	open Tony_parser
	open Helping_types
}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let empty = [' ' '\t' '\r']
let escape_seq = '\\' (['n' 't' 'r' '0' '\\' '\'' '\"'] | 'x'['0'-'9' 'a'-'f']['0'-'9' 'a'-'f'])

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
        | "'" (escape_seq | [^ '\\' '\'' '\"' '\n']) "'"	{T_char_const (lexeme lexbuf).[0]}	(*can't print non latin characters*)
	| "\"" (escape_seq | [^ '\\' '\'' '\"' '\n'])* "\""	{T_string_const (lexeme lexbuf)}

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

	| '\n'	{new_line lexbuf; lexer lexbuf}
	| empty+	{lexer lexbuf}
	| "%" [^ '\n']* "\n"	{lexer lexbuf}

	| eof	{T_eof}

	| _ as chr	{ Printf.eprintf "invalid character: '%c' (ascii: %d)\n"
					chr (Char.code chr);
				  lexer lexbuf}


and comments level = parse
	| "*>"	{ if level = 0 then lexer lexbuf else comments (level-1) lexbuf }
	| "<*"	{ comments (level+1) lexbuf }
	| '\n'	{ new_line lexbuf; comments level lexbuf }
	| _ 	{ comments level lexbuf }
	| eof	{ print_endline "comments are not closed"; T_eof }

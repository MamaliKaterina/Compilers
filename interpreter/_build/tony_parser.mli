type token =
  | T_eof
  | T_and
  | T_bool
  | T_char
  | T_decl
  | T_def
  | T_else
  | T_elsif
  | T_end
  | T_exit
  | T_false
  | T_for
  | T_head
  | T_if
  | T_int
  | T_list
  | T_mod
  | T_new
  | T_nil
  | T_is_nil
  | T_not
  | T_or
  | T_ref
  | T_return
  | T_skip
  | T_tail
  | T_true
  | T_var of (string)
  | T_int_const of (int)
  | T_char_const of (char)
  | T_string_const of (string)
  | T_eq
  | T_minus
  | T_plus
  | T_times
  | T_div
  | T_cons
  | T_dif
  | T_less
  | T_greater
  | T_less_eq
  | T_greater_eq
  | T_lbracket
  | T_rbracket
  | T_assign
  | T_lsqbracket
  | T_rsqbracket
  | T_colon
  | T_semicolon
  | T_comma

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Helping_types.ast_func_def

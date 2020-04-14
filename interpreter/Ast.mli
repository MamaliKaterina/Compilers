type var = string
type oper = O_plus | O_minus | O_times | O_div | O_mod

type ast_stmt =
  | S_simple of ast_simple
  | S_return of ast_expr
  | S_for of ast_expr * ast_stmt
  | S_if of ast_expr * ast_stmt list * ast_elsif_stmt * ast_else_stmt
  | S_for of ast_simple list * ast_expr * ast_simple list * ast_stmt list

type ast_expr =
  | E_int_const of int
  | E_char_const of char
  |

val run : ast_func_def -> unit

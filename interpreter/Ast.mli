type typ = TY_int | TY_bool | TY_char | TY_array of typ | TY_list of typ
type mytyp option = Some of typ | None
type paramPas = BY_val | BY_ref
type operator = O_plus | O_minus | O_times | O_div | O_mod
type lg_operator = LO_eq | LO_dif | LO_less | LO_greater | LO_less_eq | LO_greater_eq

type ast_formal = Formal of paramPas * typ * string list

type ast_header = Header of mytyp * string * ast_formal list

type ast_func_decl = Func_decl of ast_header

type ast_var_def = Var_def of typ * string list

type ast_func_def = Func_def of ast_header * ast_def list * ast_stmt list

and ast_def =
  | F_def of ast_func_def
  | F_decl of ast_func_decl
  | V_def of ast_var_def

type ast_expr =
  | E_int_const of int
  | E_char_const of char
  |

and ast_call =

and ast_atom =

type ast_simple =

type ast_stmt =
  | S_simple of ast_simple
  | S_return of ast_expr
  | S_if of ast_expr * ast_stmt list * ast_elsif_stmt * ast_else_stmt
  | S_for of ast_simple list * ast_expr * ast_simple list * ast_stmt list

type ast_elsif_stmt = S_elsif of ast_expr * ast_stmt list * ast_elsif_stmt

type ast_else_stmt = S_else of ast_stmt list

val run : ast_func_def -> unit

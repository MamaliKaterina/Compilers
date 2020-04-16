type typ = TY_int | TY_bool | TY_char | TY_array of typ | TY_list of typ
type paramPas = BY_val | BY_ref
type operator = O_plus | O_minus | O_times | O_div | O_mod
type lg_operator = LO_eq | LO_dif | LO_less | LO_greater | LO_less_eq | LO_greater_eq
type bool_val = True | False
type and_or = And | Or

type ast_formal = Formal of paramPas * typ * string list

type ast_header = Header of typ option * string * ast_formal list

type ast_func_decl = Func_decl of ast_header

type ast_var_def = Var_def of typ * string list

type ast_expr =
  | E_atom of ast_atom
  | E_int_const of int
  | E_char_const of char
  | E_un_plus of ast_expr
  | E_un_minus of ast_expr
  | E_op of ast_expr * operator * ast_expr
  | E_lg_op of ast_expr * lg_operator * ast_expr
  | E_bool of bool_val
  | E_not of ast_expr
  | E_and_or of ast_expr * and_or * ast_expr
  | E_new of typ * ast_expr
  | E_nil of unit
  | E_is_nil of ast_expr
  | E_cons of ast_expr * ast_expr
  | E_head of ast_expr
  | E_tail of ast_expr

and ast_call = C_call of string * ast_expr list

and ast_atom =
  | A_var of string
  | A_string_const of string
  | A_atom of ast_atom * ast_expr
  | A_call of ast_call

type ast_simple =
  | S_skip of unit
  | S_assign of ast_atom * ast_expr
  | S_call of ast_call

type ast_stmt =
  | S_simple of ast_simple
  | S_exit of unit
  | S_return of ast_expr
  | S_if of ast_expr * ast_stmt list * ast_elsif_stmt * ast_else_stmt
  | S_for of ast_simple list * ast_expr * ast_simple list * ast_stmt list

and ast_elsif_stmt = S_elsif of ast_expr * ast_stmt list * ast_elsif_stmt

and ast_else_stmt = S_else of ast_stmt list

type ast_func_def = Func_def of ast_header * ast_def list * ast_stmt list

and ast_def =
  | F_def of ast_func_def
  | F_decl of ast_func_decl
  | V_def of ast_var_def

val run : ast_func_def -> unit

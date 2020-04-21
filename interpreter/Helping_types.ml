type helping_type = 
	| Empty
	| M_int of int 
	| M_char of char 
	| M_bool of bool
	| M_array of helping_type array
	| M_list of helping_type list
	| M_name of string

type typ = TY_int | TY_bool| TY_char | TY_array of typ | TY_list of typ
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
  | E_nil
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
  | S_if of ast_expr * ast_stmt list * ast_elsif_stmt option * ast_else_stmt option
  | S_for of ast_simple list * ast_expr * ast_simple list * ast_stmt list

and ast_elsif_stmt = S_elsif of ast_expr * ast_stmt list * ast_elsif_stmt option

and ast_else_stmt = S_else of ast_stmt list

type ast_func_def = Func_def of ast_header * ast_def list * ast_stmt list

and ast_def =
  | F_def of ast_func_def
  | F_decl of ast_func_decl
  | V_def of ast_var_def

let equal v1 v2 =
	match v1, v2 with
	| (M_int(n1), M_int(n2))		-> M_bool (n1 = n2)
	| (M_char(c1), M_char(c2))		-> M_bool (c1 = c2)
	| (M_bool(b1), M_bool(b2))		-> M_bool (b1 = b2)

let not_equal v1 v2 =
	match v1, v2 with
	| (M_int(n1), M_int(n2))		-> M_bool (n1 <> n2)
	| (M_char(c1), M_char(c2))		-> M_bool (c1 <> c2)
	| (M_bool(b1), M_bool(b2))		-> M_bool (b1 <> b2)

let less v1 v2 =
	match v1, v2 with
	| (M_int(n1), M_int(n2))		-> M_bool (n1 < n2)
	| (M_char(c1), M_char(c2))		-> M_bool (c1 < c2)
	| (M_bool(b1), M_bool(b2))		-> M_bool (b1 < b2)

let greater v1 v2 =
	match v1, v2 with
	| (M_int(n1), M_int(n2))		-> M_bool (n1 > n2)
	| (M_char(c1), M_char(c2))		-> M_bool (c1 > c2)
	| (M_bool(b1), M_bool(b2))		-> M_bool (b1 > b2)

let less_eq v1 v2 =
	match v1, v2 with
	| (M_int(n1), M_int(n2))		-> M_bool (n1 <= n2)
	| (M_char(c1), M_char(c2))		-> M_bool (c1 <= c2)
	| (M_bool(b1), M_bool(b2))		-> M_bool (b1 <= b2)

let greater_eq v1 v2 =
	match v1, v2 with
	| (M_int(n1), M_int(n2))		-> M_bool (n1 >= n2)
	| (M_char(c1), M_char(c2))		-> M_bool (c1 >= c2)
	| (M_bool(b1), M_bool(b2))		-> M_bool (b1 >= b2)

let rec print_helping_type t =
	match t with
	| Empty			-> Printf.eprintf " Empty "
	| M_int (n)		-> Printf.eprintf " %d " n
	| M_char (c)	-> Printf.eprintf " %c " c 
	| M_bool (b)	-> Printf.eprintf " %b " b
	| M_array (a)	-> Printf.eprintf "{ "; Array.iter print_helping_type a; Printf.eprintf " }"
	| M_list (l)	-> Printf.eprintf "[ "; List.iter print_helping_type l; Printf.eprintf " ]"
	| M_name (s)	-> Printf.eprintf " %s " s


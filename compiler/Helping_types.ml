(*type helping_type =
	| Empty
	| M_int of int
	| M_char of char
	| M_bool of bool
	| M_array of helping_type array ref
	| M_list of helping_type list ref
  | M_name of string
*)
type typ = Null | TY_int | TY_bool| TY_char | TY_array of typ | TY_list of typ
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
  | E_un_plus of ast_expr * int
  | E_un_minus of ast_expr * int
  | E_op of ast_expr * operator * ast_expr * int
  | E_lg_op of ast_expr * lg_operator * ast_expr * int
  | E_bool of bool_val
  | E_not of ast_expr * int
  | E_and_or of ast_expr * and_or * ast_expr * int
  | E_new of typ * ast_expr * int
  | E_nil
  | E_is_nil of ast_expr * int
  | E_cons of ast_expr * ast_expr * int
  | E_head of ast_expr * int
  | E_tail of ast_expr * int

and ast_call = C_call of string * ast_expr list * int

and ast_atom =
  | A_var of string * int
  | A_string_const of string
  | A_atom of ast_atom * ast_expr * int
  | A_call of ast_call

type ast_simple =
  | S_skip of unit
  | S_assign of ast_atom * ast_expr * int
  | S_call of ast_call

type ast_stmt =
  | S_simple of ast_simple
  | S_exit of int
  | S_return of ast_expr * int
  | S_if of ast_expr * ast_stmt list * ast_elsif_stmt option * ast_else_stmt option * int
  | S_for of ast_simple list * ast_expr * ast_simple list * ast_stmt list * int

and ast_elsif_stmt = S_elsif of ast_expr * ast_stmt list * ast_elsif_stmt option * int

and ast_else_stmt = S_else of ast_stmt list

type ast_func_def = Func_def of ast_header * ast_def list * ast_stmt list

and ast_def =
  | F_def of ast_func_def
  | F_decl of ast_func_decl
  | V_def of ast_var_def

(*let equal v1 v2 =
	match v1, v2 with
	| (M_int(n1), M_int(n2))   -> M_bool (n1 = n2)
	| (M_char(c1), M_char(c2)) -> M_bool (c1 = c2)
	| (M_bool(b1), M_bool(b2)) -> M_bool (b1 = b2)

let not_equal v1 v2 =
	match v1, v2 with
	| (M_int(n1), M_int(n2))   -> M_bool (n1 <> n2)
	| (M_char(c1), M_char(c2)) -> M_bool (c1 <> c2)
	| (M_bool(b1), M_bool(b2)) -> M_bool (b1 <> b2)

let less v1 v2 =
	match v1, v2 with
	| (M_int(n1), M_int(n2))   -> M_bool (n1 < n2)
	| (M_char(c1), M_char(c2)) -> M_bool (c1 < c2)
	| (M_bool(b1), M_bool(b2)) -> M_bool (b1 < b2)

let greater v1 v2 =
	match v1, v2 with
	| (M_int(n1), M_int(n2))   -> M_bool (n1 > n2)
	| (M_char(c1), M_char(c2)) -> M_bool (c1 > c2)
	| (M_bool(b1), M_bool(b2)) -> M_bool (b1 > b2)

let less_eq v1 v2 =
	match v1, v2 with
	| (M_int(n1), M_int(n2))   -> M_bool (n1 <= n2)
	| (M_char(c1), M_char(c2)) -> M_bool (c1 <= c2)
	| (M_bool(b1), M_bool(b2)) -> M_bool (b1 <= b2)

let greater_eq v1 v2 =
	match v1, v2 with
	| (M_int(n1), M_int(n2))   -> M_bool (n1 >= n2)
	| (M_char(c1), M_char(c2)) -> M_bool (c1 >= c2)
  | (M_bool(b1), M_bool(b2)) -> M_bool (b1 >= b2)
*)
let op_as_string op =
  match op with
  | O_plus  -> "+"
  | O_minus -> "-"
  | O_times -> "*"
  | O_div   -> "/"
  | O_mod   -> "mod"

let lg_as_string lg =
  match lg with
  | LO_eq         -> "="
  | LO_dif        -> "<>"
  | LO_less       -> "<"
  | LO_greater    -> ">"
  | LO_less_eq     -> "<="
  | LO_greater_eq -> ">="

let andor_as_string ao =
  match ao with
  | And -> "and"
  | Or  -> "or"

let rec print_typ t =
  match t with
	| Null			   -> Printf.eprintf "Null\n"
	| TY_int		   -> Printf.eprintf "TY_int\n"
	| TY_char	     -> Printf.eprintf "TY_char\n"
	| TY_bool	     -> Printf.eprintf "TY_bool\n"
	| TY_array (a) -> Printf.eprintf "TY_array of "; print_typ a
  | TY_list (l)	 -> Printf.eprintf "TY_list of "; print_typ l

let rec sizeOfType t =
  match t with
  | TY_int     -> 2
  | TY_char    -> 1
  | TY_bool    -> 1
  | TY_array a -> 8 (*size of system word length, use 8 for 64-bit architecture???*)
  | TY_list l  -> 8 (*size of system word length, use 8 for 64-bit architecture???*)
  | _          -> 0

let equalType t1 t2 =
  t1 = t2

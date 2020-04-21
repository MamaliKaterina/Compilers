open Helping_types
open Tony_ActRec
open Tony_ast

let ast = Func_def (Header(None, "main", []), [V_def (Var_def (TY_int, ["number"]))], [S_simple (S_assign (A_var ("number"), E_int_const (0)))])

let main =
	run ast;
	print_act_rec ();
	exit 0

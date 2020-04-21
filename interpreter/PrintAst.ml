open Helping_types

let rec print_ast ast =
	match ast with
	| Func_def (h, defs, stmts)	-> Printf.eprintf "Func_def ( "; print_header h; Printf.eprintf " )\n";
									List.iter print_def defs;
									List.iter print_stmt stmts
and print_def def = 
	match def with
	| F_def (f) 	-> Printf.eprintf "F_def ( "; print_ast f; Printf.eprintf " )\n"
  	| F_decl (f)	-> Printf.eprintf "F_decl ( "; print_decl f; Printf.eprintf " )\n"
  	| V_def (v)		-> Printf.eprintf "V_def ( "; print_var v; Printf.eprintf " )\n"

and print_decl f =
	match f with
	| Func_decl (h)	-> Printf.eprintf "Func_decl ( "; print_header h; Printf.eprintf " )\n"

and print_var v =
	match v with
	| Var_def (t, sl)	-> Printf.eprintf "Var_def ( "; print_typ t; List.iter print_string sl; Printf.eprintf " )\n"

and print_string s =
	Printf.eprintf "%s" s; Printf.eprintf " "

and print_typ t =
	match t with
	| TY_int		-> Printf.eprintf " int "
	| TY_bool		-> Printf.eprintf " bool "
	| TY_char 		-> Printf.eprintf " char "
	| TY_array (t) 	-> print_typ t; Printf.eprintf "array "
	| TY_list (t)	-> print_typ t; Printf.eprintf "list "

and print_header h =
	match h with
	| Header (t, nm, frl)	-> Printf.eprintf "Header ( "; print_typ_op t; Printf.eprintf "%s" nm; Printf.eprintf " ";
								List.iter print_formal frl; Printf.eprintf " )" 

and print_typ_op t =
	match t with
	| Some (v)	-> print_typ v
	| None		-> ()

and print_formal fr =
	match fr with
	| Formal (m, t, sl)	-> Printf.eprintf "Formal ( "; print_paramPas m; Printf.eprintf " ";
							print_typ t; List.iter print_string sl; Printf.eprintf " )"

and print_paramPas m =
	match m with
	| BY_val	-> ()
	| BY_ref	-> Printf.eprintf " ref "

and print_stmt st =
	match st with
	| S_simple (s)							-> Printf.eprintf "S_simple ( "; print_simple s; Printf.eprintf " )\n"
  	| S_exit ()								-> Printf.eprintf "S_exit ()\n"
  	| S_return (ex)							-> Printf.eprintf "S_return ( ";  print_exp ex; Printf.eprintf " )\n"
  	| S_if (ex, stmtl, elsif_op, else_op)	-> Printf.eprintf "S_if ( ";  print_exp ex; List.iter print_stmt stmtl; 
												print_elsif_op elsif_op; print_else_op else_op;	Printf.eprintf " )\n"
  	| S_for (sl1, ex, sl2, stmtl)			-> Printf.eprintf "S_for ( "; List.iter print_simple sl1; print_exp ex;
													 List.iter print_simple sl2; List.iter print_stmt stmtl; Printf.eprintf " )\n"

and print_simple s =
	match s with
	| S_skip ()			-> Printf.eprintf "S_skip ()\n"
  	| S_assign (at, ex)	-> Printf.eprintf "S_assign ( "; print_atom at; print_exp ex; Printf.eprintf " )\n" 
  	| S_call (c)		-> Printf.eprintf "S_call ( "; print_call c; Printf.eprintf " )\n"

and print_atom at =
	match at with
	| A_var (s)				-> Printf.eprintf "A_var ( "; Printf.eprintf "%s" s; Printf.eprintf " )"
 	| A_string_const (s)	-> Printf.eprintf "A_string_const"; Printf.eprintf "%s" s; Printf.eprintf " )"
  	| A_atom (at, ex)		-> Printf.eprintf "A_atom ( "; print_atom at; print_exp ex; Printf.eprintf " ) "
  	| A_call (c)			-> Printf.eprintf "A_call ( "; print_call c; Printf.eprintf " )"

and print_exp ex =
	| E_atom (at)				-> Printf.eprintf "E_atom ( "; print_atom at; Printf.eprintf " ) "
	| E_int_const (n)			-> Printf.eprintf "E_int_const (%d) " n 
  	| E_char_const (c)			-> Printf.eprintf "E_char_const (%c)" c
  	| E_un_plus (ex)			-> Printf.eprintf "E_un_plus ("; print_exp ex; Printf.eprintf ") "
  	| E_un_minus (ex)			-> Printf.eprintf "E_un_minus ("; print_exp ex; Printf.eprintf ") "
  	| E_op (ex1, op, ex2)		-> Printf.eprintf "E_op ( "; print_exp ex1; print_op op; print_exp ex2; Printf.eprintf " )"
  	| E_lg_op (ex1, lgop, ex2)	-> Printf.eprintf "E_lg_op ( "; print_exp ex1; print_lgop lgop; print_exp ex2; Printf.eprintf " )"
  	| E_bool (b)				-> Printf.eprintf "E_bool %b" b
  	| E_not (ex)				-> Printf.eprintf "E_not ( "; print_exp ex; Printf.eprintf " ) "
  	| E_and_or of ast_expr * and_or * ast_expr
  	| E_new of typ * ast_expr
  	| E_nil
  	| E_is_nil of ast_expr
  	| E_cons of ast_expr * ast_expr
  	| E_head of ast_expr
  	| E_tail of ast_expr






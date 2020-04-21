open Helping_types
open Tony_ActRec


let string_to_array str =
	let n = ref ((String.length str) - 1)
	and ar = Array.make (String.length str) Empty in
	while !n >= 0 do
		ar.(!n) <- M_char str.[!n] ;
		n := !n - 1
	done ;
	M_array ar

let run_func_def ast =
	match ast with
	| Func_def (Header(_, nm, _), _, _) -> insert_fun_def nm ast

let run_func_decl ast =
	match ast with
	| Func_decl (Header(_, nm, _)) -> insert_fun_decl nm

let run_var_def ast = 
	match ast with
	| Var_def(t, s_list)	-> List.iter (insert_var t) s_list

let run_def ast =
  match ast with
  | F_def fdef   -> run_func_def fdef
  | F_decl fdecl -> run_func_decl fdecl
  | V_def vdef   -> run_var_def vdef


let rec run_expr ast = 
  match ast with
  | E_atom a              -> run_atom a
  | E_int_const n         -> M_int n
  | E_char_const c        -> M_char c
  | E_un_plus e           -> run_expr e 	(*will have the same sign anyway*)
  | E_un_minus e          -> let v = run_expr e in ( match v with
													 | M_int(v)	-> M_int (-v)	(*operands for arithmetic operators only of type int*)
													)
  | E_op (e1, op, e2)     -> let v1 = run_expr e1
                             and v2 = run_expr e2 in
            		             (match v1, v2, op with
            		             | M_int(v), M_int(m), O_plus  -> M_int (v + m)
            		             | M_int(v), M_int(m), O_minus -> M_int (v - m)
                             	 | M_int(v), M_int(m), O_times -> M_int (v * m)
                             	 | M_int(v), M_int(m), O_div   -> M_int (v / m)
                             	 | M_int(v), M_int(m), O_mod   -> M_int (v mod m) )
  | E_lg_op (e1, op, e2)  -> let v1 = run_expr e1
                             and v2 = run_expr e2 in
            		             (match op with
            		             | LO_eq 			-> equal v1 v2
            		             | LO_dif			-> not_equal v1 v2
                             	 | LO_less			-> less v1 v2
                             	 | LO_greater 		-> greater v1 v2
                             	 | LO_less_eq 		-> less_eq v1 v2
                             	 | LO_greater_eq 	-> greater_eq v1 v2 )
  | E_bool b              -> (match b with
                             | True  -> M_bool (true)
                             | False -> M_bool (false) )
  | E_not e               -> let v = run_expr e in (match v with
													| M_bool(m)	-> M_bool (not m))
  | E_and_or (e1, ao, e2) -> let v1 = run_expr e1
                             and v2 = run_expr e2 in
                             (match v1, v2, ao with
                             | M_bool(v), M_bool(m), And -> M_bool (v && m)
                             | M_bool(v), M_bool(m), Or  -> M_bool (v || m) )
  | E_new (_, e)          -> let s = run_expr e in 
								(match s with
								| M_int(n)	-> M_array (Array.make n Empty))
  | E_nil 	              -> M_list []
  | E_is_nil e            -> let v = run_expr e in
								(match v with
								 | M_list(l)	-> M_bool (l == []))
  | E_cons (e1, e2)       -> let v1 = run_expr e1
                             and v2 = run_expr e2 in (match v2 with
													  | M_list(m)  ->  M_list(v1::m) )
  | E_head e              -> let v = run_expr e in (match v with
													| M_list(l)	-> List.hd l )
  | E_tail e              -> let v = run_expr e in (match v with
													| M_list(l)	-> M_list (List.tl l) )

and run_atom ast =
  match ast with
  | A_var v            -> get_var (v, Empty)
  | A_string_const str -> string_to_array str
  | A_atom (a, e)      -> let n = run_expr e in	(*array 2-D???*)
							( match a with
							  | A_var(v)	->
							get_var (v, n) )
  | A_call c           -> run_call c

and init_par exl frl =
	match exl, frl with
	| ([], [])				-> ()
	| (l, hd2::tl2)	-> match hd2 with
						| Formal (m, t, s_list)	-> let tl = pass_param l s_list m t in init_par tl tl2

and pass_param exl stl mode t =
	match exl, stl with
	| (tl, [])	-> tl
	| (hd1::tl1, hd2::tl2)	-> match mode with
							   | BY_val	-> (let r = run_expr hd1 in
											 insert_var t hd2;
											 update_var (hd2, Empty) r ;	(*??*)
											 pass_param tl1 tl2 mode t )
							   | BY_ref	-> (match !cur_act_rec.previous_act_rec , hd1 with
											| (Some(act_rec), E_atom(at))	-> let a = run_atom_assign at in
																				let r = get_val_ref a act_rec in
																	    			insert_ref_var t hd2 r ;
																	    			pass_param tl1 tl2 mode t)

and run_call ast = 
	match ast with
	| C_call (nm, exp_list)	-> let fun_body = access_fun nm in
								new_act_rec () ;
								(match fun_body with
								 | Func_def ( Header ( _, nm, form_list), _, _) ->
								   init_par exp_list form_list) ;
								run_func fun_body ;
								get_return_value ()

(*This will change to return a reference to a helping_type*)
and run_atom_assign ast = 
	match ast with
	| A_var	v		-> (v, Empty)
	| A_atom (a, e)	-> let n = run_expr e in 
						match a with
						| A_var(s)	-> (s, n)			(*it could return only int...*)
	

and run_call_general ast =
	let res = run_call ast in
	match res with
	| Empty -> ()
	(*| _ -> "error?"*)

and run_simple ast =
  match ast with
  | S_skip ()       -> ()
  | S_assign (a, e) -> let x = run_atom_assign a
                       and y = run_expr e in
                       update_var x y (*I want to put values in current activation record*)
  | S_call c        -> run_call_general c	(*It might return sth...*)



and run_stmt ast =
  match ast with
  | S_simple s                         -> run_simple s
  | S_exit ()                          -> remove_act_rec Empty; ()
  | S_return e                         -> let v = run_expr e in 
											remove_act_rec v; ()
  | S_if (e, stmts, elsif, els)        -> let var = run_expr e in
										   (match var with
											| M_bool(v) -> if v then List.iter run_stmt stmts
														   else (match elsif with
																 | Some (elif)	-> run_elsif_stmt elif els
																 | None			-> (match els with
																					| Some(es)	-> run_else_stmt es
																					| None		-> () )))
  | S_for (simples1, e, simples2, stmts) -> (List.iter run_simple simples1; 
										  	(let var = run_expr e in
											(match var with
                                          	| M_bool(v) -> (let b = ref v in
                                          					while !b do (*something is wrong with s*)
                                            					List.iter run_stmt stmts;
                                            					List.iter run_simple simples2;
																let var2 = run_expr e in
																match var2 with
																| M_bool(v2)	-> b := v2
                                          					done ))))

and run_elsif_stmt ast els =
  match ast with
  | S_elsif (e, stmts, elsif) -> let var = run_expr e in
									match var with
                                 	|M_bool(v)	-> if v then List.iter run_stmt stmts
                                           		   else (match elsif with
														 | Some (elif)	-> run_elsif_stmt elif els
														 | None			-> (match els with
																					| Some(es)	-> run_else_stmt es
																					| None		-> () ))


and run_else_stmt ast =
  match ast with
  | S_else stmts -> List.iter run_stmt stmts


and run_func ast = 
  match ast with
  | Func_def (_, defs, stmts) -> List.iter run_def defs;
								 List.iter run_stmt stmts
      
let run ast = run_func ast

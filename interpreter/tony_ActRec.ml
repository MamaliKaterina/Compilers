open Helping_types

type activation_record = {
	mutable return_value: 			  helping_type option;
	mutable variable_parameters: 	parameters list;
	mutable function_parameters: 	fun_param list;
	previous_act_rec:				      activation_record option
}

and parameters = {
	name:				        string;
	mtype:				      typ;
	mutable mode:	     	paramPas;
	mutable value:		  helping_type ref;
	mutable reference:	helping_type ref
}


and fun_param = {
	fname: 			string;
	mutable body:	ast_func_def option
}


let outer_act_rec = {
	return_value = None;
	variable_parameters = [];
	function_parameters = [];
	previous_act_rec = None
}

let cur_act_rec = ref outer_act_rec

let print_return_value v =
	match v with
	| None 		  -> ()
	| Some (n)	-> Printf.eprintf "return value: "; print_helping_type n

let print_parameter p =
	Printf.eprintf "name: %s, value:" p.name;
	match p.mode with
  | BY_val	-> print_helping_type !(p.value); Printf.eprintf "\n"
	| BY_ref	-> print_helping_type !(p.reference); Printf.eprintf "\n"

let print_fun f =
	Printf.eprintf "%s" f.fname; Printf.eprintf "\n"

let print_act_rec () =
	print_return_value !cur_act_rec.return_value;
	Printf.eprintf "Variables: "; List.iter print_parameter !cur_act_rec.variable_parameters;
	Printf.eprintf "Function: "; List.iter print_fun !cur_act_rec.function_parameters

let new_act_rec () =
	let act_rec = {
		return_value = None;
		variable_parameters = [];
		function_parameters = [];
		previous_act_rec = Some !cur_act_rec
	} in
	cur_act_rec := act_rec

let remove_act_rec () =
	match !cur_act_rec.previous_act_rec with
	| Some(act_rec)	-> (let vl = !cur_act_rec.return_value in
	                    cur_act_rec := act_rec;
                      (match vl with
                       | None -> Empty
                       | Some(v) -> v))
	

let put_return_value vl =
  !cur_act_rec.return_value <- Some vl

let get_return_value () =
	match !cur_act_rec.return_value with
	| Some (v)	-> v
	| None			-> Empty

let insert_var t n =
	let par = {
			name = n;
			mtype = t;
			mode = BY_val;
			value = ref Empty;
			reference = ref Empty
		} in
	!cur_act_rec.variable_parameters <- par :: !cur_act_rec.variable_parameters

let rec find_val nm l =
	match l with
	| []		  -> None
	| hd::tl	->  if hd.name = nm then (match hd.mode with
										                  | BY_val	-> Some (hd.value)
										                  | BY_ref	-> Some (hd.reference) )
					      else find_val nm tl

(*let rec find_array_val nm pos l =
	match l with
	| []		  ->  None
	| hd::tl	->  if hd.name = nm then (match hd.mode with
										                  | BY_val	-> (match hd.value with
													                          | M_array(ar)	-> Some (ref ar.(pos)) )
										                  | BY_ref	-> (match !(hd.reference) with
													                          | M_array(ar)	-> Some (ref ar.(pos)) ) )
  else find_val nm tl*)

let rec get_val_ref nm act_rec =
	(*match at with
   | (nm, Empty) 		-> ( *)
   let var = find_val nm act_rec.variable_parameters in
		match var with
		| Some(var_ref)	-> var_ref
		| None			    ->  match act_rec.previous_act_rec with
			 								  | Some(p_act_rec)	-> get_val_ref nm p_act_rec
(*)| (nm, M_int(n))	-> (let var = find_array_val nm n act_rec.variable_parameters in
								        match var with
								        | Some(var_ref)	-> var_ref
								        | None			    -> match act_rec.previous_act_rec with
			 									                   | Some(p_act_rec)	-> get_val_ref at p_act_rec)*)



let insert_ref_var t ref_nm par =
		let ref_par = {
			name = ref_nm;
			mtype = t;
			mode = BY_ref;
			value = ref Empty;
			reference = par
		} in
	!cur_act_rec.variable_parameters <- ref_par :: !cur_act_rec.variable_parameters

(*let rec set_var n vl l =
	match l with
	| []		->	false
	| hd::tl	->  if hd.name = n then (match hd.mtype with
										                 | TY_int | TY_char | TY_bool	-> (match hd.mode with
																		                                  | BY_val	-> (hd.value <- vl ; true)
																			                                | BY_ref	->  hd.reference := vl ; true)
										                 | TY_array(_)	| TY_list(_)	->
												             (match vl with
												              | M_name (a)	->  (let par = get_val_ref (a, Empty) !cur_act_rec in
																				                  hd.mode <- BY_ref;
																				                  hd.reference <- par ; true)
												              | _			->  ( match hd.mode with
																                    | BY_val	-> hd.value <- vl ; true
																                    | BY_ref	-> hd.reference := vl ; true) ) )
					      else set_var n vl tl

let rec find_and_set_var n vl act_rec =
	if set_var n vl act_rec.variable_parameters then ()
	else	 match act_rec.previous_act_rec with
			   | Some(p_act_rec)	-> find_and_set_var n vl p_act_rec

let rec found_array nm pos vl l =
	match l with
	| []		  -> false
	| hd::tl	-> if hd.name = nm then (match hd.mode with
										                 | BY_val	-> (match hd.value with
														                      | M_array(ar)	-> ar.(pos) <- vl ; true)
										                 | BY_ref	-> (match !(hd.reference) with
														                      | M_array(ar)	-> ar.(pos) <- vl ; true))
				       else found_array nm pos vl tl

let rec update_array_var nm pos vl act_rec =
	if found_array nm pos vl act_rec.variable_parameters then ()
	else	match act_rec.previous_act_rec with
			  | Some(p_act_rec)	-> update_array_var nm pos vl p_act_rec

let update_var nm vl =
	match nm with
	| (s, Empty)	  ->	find_and_set_var s vl !cur_act_rec
 | (s, M_int(n))	->	update_array_var s n vl !cur_act_rec*)
	(*| _				    -> internal "error"*)

(*let rec find_array nm pos l =
	match l with
	| []		  -> None
	| hd::tl	-> if hd.name = nm then (match hd.mode with
										                 | BY_val	-> (match hd.value with
														                      | M_array(ar)	-> Some ar.(pos) )
				   						               | BY_ref	-> (match !(hd.reference) with
														                      | M_array(ar)	-> Some ar.(pos) ) )
				       else find_array nm pos tl

let rec access_array_var nm pos act_rec =
	let res = find_array nm pos act_rec.variable_parameters in
	match res with
	| Some(v)	-> v
	| None		-> match act_rec.previous_act_rec with
   | Some(p_act_rec)	-> access_array_var nm pos p_act_rec*)


let rec return_var n l =
	match l with
	| []		  ->	None
	| hd::tl	->  if hd.name = n then (match hd.mode with
                                     | BY_val	-> Some !(hd.value)
																		 | BY_ref	-> Some !(hd.reference) )
     (*| TY_array(_)	| TY_list(_)	-> Some (M_name n)*)
					      else return_var n tl

let rec find_and_return_var n act_rec =
	let res = return_var n act_rec.variable_parameters in
	match res with
	| Some(v)	-> v
	| None		-> match act_rec.previous_act_rec with
			         | Some(p_act_rec)	-> find_and_return_var n p_act_rec

let get_var var =
  find_and_return_var var !cur_act_rec
	(*match var with
	| (nm, Empty)		  -> find_and_return_var nm !cur_act_rec
 | (nm, M_int(n))	-> access_array_var nm n !cur_act_rec*)

let insert_fun_decl n =
	let par = {
		fname = n;
		body = None
	} in
	!cur_act_rec.function_parameters <- par :: !cur_act_rec.function_parameters


let rec update_fun n ast el =
	if el.fname = n then
		el.body <- Some ast

let rec found_fun n l =
	match l with
	| [] 	   ->  false
	| hd::tl ->	 if hd.fname = n then true
				       else found_fun n tl

let insert_fun_def n ast =
	if found_fun n !cur_act_rec.function_parameters then List.iter (update_fun n ast) !cur_act_rec.function_parameters
	else
		let par = {
			fname = n;
			body = Some ast
		} in
		!cur_act_rec.function_parameters <- par :: !cur_act_rec.function_parameters

let rec search_fun n l =
	match l with
	| []		  -> None
	| hd::tl	-> if hd.fname = n then hd.body
				   	   else	search_fun n tl

let rec find_fun n act_rec =
	let res = search_fun n act_rec.function_parameters in
	match res with
	| Some (ast)	-> ast
	| None		    -> match act_rec.previous_act_rec with
					         | Some(p_act_rec)	-> find_fun n p_act_rec

let access_fun n =
	find_fun n !cur_act_rec

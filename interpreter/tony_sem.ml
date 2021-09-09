open Identifier
open Error
open Helping_types
open Tony_symbol

exception UnknownError (*use this when the error is unexpected and cannot understand why came up*)
exception TypeError of int

let invalid_log_op t1 t2 =
  match t1, t2 with
  | (TY_int, TY_int) | (TY_char, TY_char) | (TY_bool, TY_bool) -> false
  | _ -> true

let sem_formal f formal =
  match formal with
  | Formal(parPas, t, slist) -> let idlist = List.rev_map id_make slist in
    (match parPas with
     | BY_val -> let rec callnewParam idlist =
                   (match idlist with
                    | [id] -> let dummy = newParameter true f PASS_BY_VALUE t (List.hd idlist) in ()
                    | _    -> (let dummy = newParameter true f PASS_BY_VALUE t (List.hd idlist) in
                               callnewParam (List.tl idlist)) )
       in callnewParam idlist
     | BY_ref -> let rec callnewParam idlist =
                   (match idlist with
                    | [id] -> let dummy = newParameter true f PASS_BY_REFERENCE t (List.hd idlist) in ()
                    | _    -> (let dummy = newParameter true f PASS_BY_REFERENCE t (List.hd idlist) in
                               callnewParam (List.tl idlist)) )
       in callnewParam idlist )

let rec sem_func_def ast =
  match ast with
  | Func_def (Header(tOp, nm, formals), defs, stmts) -> let id = id_make nm in
    let f = newFunction id true in
    (match tOp with
     | Some(t) -> openScope t;
       List.iter (sem_formal f) formals;
       endFunctionHeader f t
     | None    -> openScope Null;
       List.iter (sem_formal f) formals;
       endFunctionHeader f Null );
    (*maybe announce parameters as variables here*)
    List.iter sem_def defs;
    List.iter sem_stmt stmts;
    closeScope ()

and sem_func_decl ast =
  (*must keep the fact that the function must be properly defined!!!*)
  match ast with
  | Func_decl (Header(tOp, nm, formals)) -> let id = id_make nm in
    let f = newFunction id true in
    List.iter (sem_formal f) formals;
    forwardFunction f;
    (match tOp with
     | Some(t) -> endFunctionHeader f t
     | None    -> endFunctionHeader f Null )

and sem_var_def ast =
  match ast with
  | Var_def(t, s_list)	-> let idlist = List.rev_map id_make s_list in
    let rec callnewParam idlist =
      (match idlist with
       | [id] -> let d = newVariable true t (List.hd idlist) in ()
       | _    -> (let d = newVariable true t (List.hd idlist) in
                  callnewParam (List.tl idlist)) )
    in callnewParam idlist

and sem_def ast =
  match ast with
  | F_def fdef   -> sem_func_def fdef
  | F_decl fdecl -> sem_func_decl fdecl
  | V_def vdef   -> sem_var_def vdef

and sem_expr ast =
  (*cannot avoid raising an exception; otherwise, compiler throws error expecting type typ*)
  match ast with
  | E_atom a                    -> sem_atom a
  | E_int_const n               -> TY_int
  | E_char_const c              -> TY_char
  | E_un_plus (e, line)         -> let t = sem_expr e in
    (if t <> TY_int then (error "operator '+' expected integer as operand";
                          raise (TypeError line))
     else TY_int)
  | E_un_minus (e, line)        -> let t = sem_expr e in
    (if t <> TY_int then (error "operator '-' expected integer as operand";
                          raise (TypeError line))
     else TY_int)
  | E_op (e1, op, e2, line)     -> let t1 = sem_expr e1
    and t2 = sem_expr e2 in
    (if t1 <> TY_int || t2 <> TY_int then
       (let opString = op_as_string op in
        error "operator '%s' expected integers as operands" opString;
        raise (TypeError line))
     else TY_int)
  | E_lg_op (e1, op, e2, line)  -> let t1 = sem_expr e1
    and t2 = sem_expr e2 in
    (if invalid_log_op t1 t2 then
       (let opString = lg_as_string op in
        error "operator '%s' expected operands of same primitive type" opString;
        raise (TypeError line))
     else TY_bool)
  | E_bool b                    -> TY_bool
  | E_not (e, line)             -> let t = sem_expr e in
    (if t <> TY_bool then (error "operator 'not' expected boolean as operand";
                           raise (TypeError line))
     else TY_bool)
  | E_and_or (e1, ao, e2, line) -> let t1 = sem_expr e1
    and t2 = sem_expr e2 in
    (if t1 <> TY_bool || t2 <> TY_bool then
       (let opString = andor_as_string ao in
        error "operator '%s' expected booleans as operands" opString;
        raise (TypeError line))
     else TY_bool)
  | E_new (a, e, line)          -> let t = sem_expr e in
    (if not (a <> Null) || t <> TY_int then
       (error "operator 'new' expected a valid type and an integer as operands";
        raise (TypeError line))
     else TY_array a )
  | E_nil                       -> TY_list Null
  | E_is_nil (e, line)          -> let t = sem_expr e in
    (match t with
     | TY_list(_) -> TY_bool
     | _          -> error "operator 'nil?' expected operand of type list";
       raise (TypeError line) )
  | E_cons (e1, e2, line)       -> let v1 = sem_expr e1
<<<<<<< HEAD
    and v2 = sem_expr e2 in
    (match v2 with
     | TY_list(v) -> if v <> v1 && v <> Null then
         (error "type of list of right operand of operator '#' \
                 must be either of same type as left operand or empty list";
          raise (TypeError line))
       else v2
     | _          -> error "operator '#' expected a valid type and a list as operands";
       raise (TypeError line) )
=======
                                   and v2 = sem_expr e2 in
                                   (match v2 with
                                    | TY_list(v) -> if v <> v1 && v <> Null then
                                                      (error "type of list of right operand of operator '#' \
                                                       must be either of same type as left operand or empty list";
                                                       raise (TypeError line))
                                                    else v2
                                    | _          -> error "operator '#' expected a valid type and a list as operands";
                                                     raise (TypeError line) )
>>>>>>> master
  | E_head (e, line)            -> let v = sem_expr e in
    (match v with
     | TY_list(l) -> l
     | _          -> error "operator 'head' expected operand of type list";
       raise (TypeError line) )
  | E_tail (e, line)            -> let v = sem_expr e in
    (match v with
     | TY_list(l) -> v
     | _          -> error "operator 'nil?' expected operand of type list";
       raise (TypeError line) )

and check_param exs pars fname line =
  (*parameters by reference have to be lvalues; we do not check this!*)
  match exs, pars with
  | ([], [])       -> ()
  | (e::te, p::tp) -> let t = sem_expr e in
<<<<<<< HEAD
    (match p.entry_info with
     | ENTRY_parameter(pi) -> if pi.parameter_type <> t then
         (match pi.parameter_type, t with
          | TY_list(sth), TY_list Null -> ()
          | _                          -> (error "parameter type in call of function \
                                                  '%s' inconsistent with type \
                                                  in function definition" fname;
                                           raise (TypeError line)) )
       else if pi.parameter_mode = PASS_BY_REFERENCE then
         (match e with
          | E_atom(A_var(v, l))      -> ()
          | E_atom(A_atom(a, ex, l)) -> ()
          | _                        -> error "parameter passed by reference has to be \
                                               an lvalue in call of function '%s'" fname;
            raise (TypeError line) )
       else check_param te tp fname line
     | _                   -> raise UnknownError )
=======
                      (match p.entry_info with
                       | ENTRY_parameter(pi) -> if pi.parameter_type <> t then
                                                (match pi.parameter_type, t with
                                                 | TY_list(sth), TY_list Null -> ()
                                                 | _                          -> (error "parameter type in call of function \
                                                                                         '%s' inconsistent with type \
                                                                                         in function definition" fname;
                                                                                  raise (TypeError line)) )
                                                else if pi.parameter_mode = PASS_BY_REFERENCE then
                                                  (match e with
                                                   | E_atom(A_var(v, l))      -> ()
                                                   | E_atom(A_atom(a, ex, l)) -> ()
                                                   | _                        -> error "parameter passed by reference has to be \
                                                                                        an lvalue in call of function '%s'" fname;
                                                                                 raise (TypeError line) )
                                                else check_param te tp fname line
                       | _                   -> raise UnknownError )
>>>>>>> master
  | _              -> error "fewer or more parameters than expected in call of function '%s'" fname;
    raise (TypeError line)

and sem_call c =
  match c with
  | C_call(nm, exprs, line) -> let id = id_make nm in
<<<<<<< HEAD
    let e = lookupEntry id LOOKUP_ALL_SCOPES true in
    (match e.entry_info with
     | ENTRY_function(f) -> if not (f.function_isForward) then
         begin
           check_param exprs f.function_paramlist nm line;
           f.function_result
         end
       else (error "function '%s' is not yet defined" nm;
             raise (TypeError line))
     | _ -> error "identifier '%s' is not a function" nm;
       raise (TypeError line) )
=======
                               let e = lookupEntry id LOOKUP_ALL_SCOPES true in
                               (match e.entry_info with
                                | ENTRY_function(f) -> if not (f.function_isForward) then
                                                       begin
                                                         check_param exprs f.function_paramlist nm line;
                                                         f.function_result
                                                       end
                                                       else (error "function '%s' is not yet defined" nm;
                                                             raise (TypeError line))
                                | _ -> error "identifier '%s' is not a function" nm;
                                       raise (TypeError line) )
>>>>>>> master

and sem_atom ast =
  (*must check: -the atoms are well-defined
                -return the correct typ for each atom*)
  match ast with
  | A_var (v, line)     -> let id = id_make v in
    let e = lookupEntry id LOOKUP_ALL_SCOPES true in
    (match e.entry_info with
     | ENTRY_variable(v)  -> v.variable_type
     | ENTRY_parameter(v) -> v.parameter_type
     | ENTRY_temporary(v) -> v.temporary_type
     | _                  -> error "identifier '%s' is neither variable nor parameter" v;
       raise (TypeError line) )
  | A_string_const str  -> TY_array TY_char
  | A_atom (a, e, line) -> let v = sem_atom a
<<<<<<< HEAD
    and n = sem_expr e in
    (match v, n with
     | TY_array(t), TY_int -> t
     | _                   -> error "identifier is not an array"; (*print_typ v; print_typ //only for debugging*)
       raise (TypeError line) )
=======
                           and n = sem_expr e in
                           (match v, n with
                            | TY_array(t), TY_int -> t
                            | _                   -> error "identifier is not an array"; (*print_typ v; print_typ //only for debugging*)
                                                     raise (TypeError line) )
>>>>>>> master
  | A_call c            -> sem_call c

and sem_simple ast =
  match ast with
  | S_skip ()             -> ()
  | S_assign (a, e, line) -> let x = sem_atom a
<<<<<<< HEAD
    and y = sem_expr e in (*it will return the typ of y*)
    if x <> y then
      (match x, y with
       | TY_list(sth), TY_list Null -> ()
       | _                          -> (error "lvalue and rvalue in assignment are not of the same type";
                                        (*Printf.eprintf "%d " line; print_typ x; print_typ y;//only for debugging*)
                                        raise (TypeError line)) )
  (*else Printf.eprintf "%d " line; print_typ x; print_typ y;//else statement only for debugging*)
=======
                             and y = sem_expr e in (*it will return the typ of y*)
                             if x <> y then
                               (match x, y with
                               | TY_list(sth), TY_list Null -> ()
                               | _                          -> (error "lvalue and rvalue in assignment are not of the same type";
                                                        (*Printf.eprintf "%d " line; print_typ x; print_typ y;//only for debugging*)
                                                                raise (TypeError line)) )
                            (*else Printf.eprintf "%d " line; print_typ x; print_typ y;//else statement only for debugging*)
>>>>>>> master
  | S_call c              -> let v = sem_call c	in
    if v <> Null then raise UnknownError

and sem_stmt ast =
  match ast with
  | S_simple s                                 -> sem_simple s
  | S_exit line                                -> let rv = get_cur_return_value () in
    if rv <> Null then
      (error "exit command can only be used in void functions";
       raise (TypeError line))
    else ()
  | S_return (e, line)                         -> let t = sem_expr e
    and rv = get_cur_return_value () in
    if rv <> t then
      (error "type of object to be returned incompatible with return type of function";
       raise (TypeError line))
    else ()
  | S_if (e, stmts, elsif, els, line)          -> let var = sem_expr e in
    (match var with
     | TY_bool -> (List.iter sem_stmt stmts;
                   (match elsif with
                    | Some (elif) -> sem_elsif_stmt elif els
                    | None			   -> (match els with
                        | Some(es) -> sem_else_stmt es
                        | None		 -> () )))
     | _       -> error "condition in if-statement must be evaluated as boolean";
       raise (TypeError line) )
  | S_for (simples1, e, simples2, stmts, line) -> List.iter sem_simple simples1;
    (let var = sem_expr e in
     match var with
     | TY_bool -> List.iter sem_simple simples2;
       List.iter sem_stmt stmts
     | _       -> error "condition in for-statement must be evaluated as boolean";
       raise (TypeError line) )

and sem_elsif_stmt ast els =
  match ast with
  | S_elsif (e, stmts, elsif, line) -> let var = sem_expr e in
    (match var with
     | TY_bool	-> (List.iter sem_stmt stmts;
                   (match elsif with
                    | Some (elif) -> sem_elsif_stmt elif els
                    | None			   -> (match els with
                        | Some(es) -> sem_else_stmt es
                        | None		 -> () )))
     | _       -> error "condition in elsif-statement must be evaluated as boolean";
       raise (TypeError line) )

and sem_else_stmt ast =
  match ast with
  | S_else stmts -> List.iter sem_stmt stmts

and sem_func ast =
  match ast with
  | Func_def (Header(None, _, []), defs, stmts) -> List.iter sem_def defs;
    List.iter sem_stmt stmts
  | _ -> error "main function must be void and without arguments"

let sem ast = sem_func ast

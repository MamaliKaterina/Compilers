open Helping_types
open Tony_symbol

exception TypeError

let invalid_log_op t1 t2 =
  match t1, t2 with
  | (TY_int, TY_int) | (TY_char, TY_char) | (TY_bool, TY_bool) -> false
  | _ -> true

let sem_formals f formal =
  match formal with
  | Formal(parPas, t, slist) -> (match parPas with
                                 | BY_val -> List.iter (newParameter true f PASS_BY_VALUE t) (List.rev_map id_make slist)
                                 | BY_ref -> List.iter (newParameter true f PASS_BY_REFERENCE t) (List.rev_map id_make slist) )

let rec sem_func_def ast =
  match ast with
  | Func_def (Header(tOp, nm, formals), defs, stmts) -> let id = id_make nm in
                                                        let f = newFunction id true in
                                                          List.iter sem_formal f formals;
                                                          (match tOp with
                                                           | Some(t) -> endFunctionHeader f t;
                                                                        openScope t
                                                           | None    -> endFunctionHeader f Null;
                                                                         openScope Null );
                                                          (*maybe announce parameters as varables here*)
                                                          List.iter sem_def defs;
                                                          List.iter sem_stmt stmts;
                                                          closeScope ()


and sem_func_decl ast =
  (*must keep the fact that the function must be properly defined!!!*)
  match ast with
  | Func_decl (Header(tOp, nm, formals)) -> let id = id_make nm in
                                            let f = newFunction id true in
                                              List.iter sem_formal f formals;
                                              forwardFunction f;
                                              match tOp with
                                              | Some(t) -> endFunctionHeader f t
                                              | None    -> endFunctionHeader f Null

and sem_var_def ast =
  match ast with
  | Var_def(t, s_list)	-> List.iter (newVariable true t) (List.rev_map id_make s_list)

and sem_def ast =
  match ast with
  | F_def fdef   -> sem_func_def fdef
  | F_decl fdecl -> sem_func_decl fdecl
  | V_def vdef   -> sem_var_def vdef


and sem_expr ast =
  (*problem... it will return the typ of expression but nothing in case of TypeError->compiler fault*)
  match ast with
  | E_atom a              -> sem_atom a
  | E_int_const n         -> TY_int
  | E_char_const c        -> TY_char
  | E_un_plus e           -> let t = sem_expr e in
                             (if t <> TY_int then raise TypeError
                              else TY_int)
  | E_un_minus e          -> let t = sem_expr e in
                             (if t <> TY_int then raise TypeError
                              else TY_int)
  | E_op (e1, op, e2)     -> let t1 = sem_expr e1
                             and t2 = sem_expr e2 in
                             (if t1 <> TY_int || t2 <> TY_int then raise TypeError
                              else TY_int)
  | E_lg_op (e1, op, e2)  -> let t1 = sem_expr e1
                             and t2 = sem_expr e2 in
                             (if invalid_log_op t1 t2 then raise TypeError
                              else TY_bool)
  | E_bool b              -> TY_bool
  | E_not e               -> let t = sem_expr e in
                             (if t <> TY_bool then raise TypeError
                              else TY_bool)
  | E_and_or (e1, ao, e2) -> let t1 = sem_expr e1
                             and t2 = sem_expr e2 in
                             (if t1 <> TY_bool || t2 <> TY_bool then raise TypeError
                              else TY_bool)
  | E_new (a, e)          -> let a = sem_expr a
                             and t = sem_expr e in
                             (if a <> TY_array(_) || t <> TY_int (*must be positive->runtime check!!!*) then raise TypeError
                              else a )
  | E_nil 	              -> TY_list Null
  | E_is_nil e            -> let t = sem_expr e in
                             (if t <> TY_list(_) then raise TypeError
                              else TY_bool )
  | E_cons (e1, e2)       -> let v1 = sem_expr e1
                             and v2 = sem_expr e2 in
                             (match v2 with
                              | TY_list(v1)  ->  v2
                              | _            -> raise TypeError )
  | E_head e              -> let v = sem_expr e in
                             (match v with
                              | TY_list(l)	-> l
                              | _         -> raise TypeError )
  | E_tail e              -> let v = sem_expr e in
                             (match v with
                              | TY_list(l)	-> v
                              | _           -> raise TypeError )

and check_param exs pars =
  match exs, pars with
  | ([], [])       -> ()
  | (e::te, p::tp) -> let t = sem_expr e in
                      (match p with
                       | ENTRY_parameter(pi) -> if pi.parameter_type <> t then raise TypeError
                                                else ()
                       | _ -> raise TypeError )
  | _              -> raise TypeError

and sem_call c =
  match c with
  | C_call(nm, exprs) -> let id = id_make nm in
                         let e = lookupEntry id LOOKUP_ALL_SCOPES true in
                         (match e.entry_info with
        | ENTRY_function(f) -> if not (f.function_isForward) then
                                   begin
                                     check_param exprs f.function_paramlist;
                                     f.function_result
                                   end
                              else raise TypeError
                          | _ -> raise TypeError)

and sem_atom ast =
  (*must check: -the atoms are well-defined
                -return the correct typ for each atom*)
  match ast with
  | A_var v            -> let id = id_make v in
                          let e = lookupEntry id LOOKUP_ALL_SCOPES true in
                          (match e.entry_info with
                           | ENTRY_variable(v)  -> v.variable_type
                           | ENTRY_parameter(v) -> v.parameter_type
                           | ENTRY_temporary(v) -> v.temporary_type
                           | _                  -> raise TypeError )
  | A_string_const str -> TY_array TY_char
  | A_atom (a, e)      -> let v = sem_atom a
                          and n = sem_expr e in
                          (match v, n with
                           | (TY_array(t), TY_int)	-> t
                           | _                      -> raise TypeError )
  | A_call c           -> sem_call c


and sem_simple ast =
  match ast with
  | S_skip ()       -> ()
  | S_assign (a, e) -> let x = sem_atom a
                       and y = sem_expr e in (*it will return the typ of y*)
                       if x <> y then raise TypeError
  | S_call c        -> let v = sem_call c	in
                        if v <> Null then raise TypeError


and sem_stmt ast =
  if valid () then (
    match ast with
    | S_simple s                         -> sem_simple s
    | S_exit ()                          -> let rv = get_cur_return_value () in
                                               if rv <> Null then raise TypeError
                                               else ()
    | S_return e                         -> let t = sem_expr e
                                            and rv = get_cur_return_value () in
                                            if rv <> t then raise TypeError
                                            else ()
    | S_if (e, stmts, elsif, els)        -> let var = sem_expr e in
                                            (match var with
                                             | TY_bool -> (List.iter sem_stmt stmts;
                                                          (match elsif with
                                                           | Some (elif)	-> sem_elsif_stmt elif els
                                                           | None			-> (match els with
                                                                          | Some(es)	-> sem_else_stmt es
                                                                          | None		-> () )))
                                             | _       -> raise TypeError )
    | S_for (simples1, e, simples2, stmts) -> List.iter sem_simple simples1;
                                              (let var = sem_expr e in
                                                match var with
                                                  | TY_bool -> List.iter sem_simple simples2;
                                                               List.iter sem_stmt stmts
                                                  | _       -> raise TypeError )


and sem_elsif_stmt ast els =
  match ast with
  | S_elsif (e, stmts, elsif) -> let var = sem_expr e in
                                 (match var with
                                  | TY_bool	-> (List.iter sem_stmt stmts;
                                                (match elsif with
                                                 | Some (elif)	-> sem_elsif_stmt elif els
                                                 | None			-> (match els with
                                                                | Some(es)	-> sem_else_stmt es
                                                                | None		-> () )))
                                  | _       -> raise TypeError )


and sem_else_stmt ast =
  match ast with
  | S_else stmts -> List.iter sem_stmt stmts


and sem_func ast =
  match ast with
  | Func_def (Header(None, _, []), defs, stmts) -> List.iter sem_def defs;
                                                   List.iter sem_stmt stmts
  | _ -> raise TypeError (*maybe create another kind of error...*)

let sem ast = sem_func ast

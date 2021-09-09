open Identifier
open Error
open Helping_types
open Tony_symbol

exception UnknownError (*use this when the error is unexpected and cannot understand why came up*)
exception TypeError of int

let insert_built_in_function nm =
  let id =  id_make nm in
  (*create fake scope for parameters*)
  let sco = {
    sco_parent = None;
    sco_nesting = -1;
    sco_entries = [];
    sco_negofs = 8;
    return_value = Null;
    fun_name = Some("pre-main")
  } in
  let (par, res) = (
    match nm with
    | "puti" -> (
        let inf_p = {
          parameter_type = TY_int;
          parameter_offset = 0;
          parameter_mode = PASS_BY_VALUE;
          parameter_value = None
        } in
        let p = {
          entry_id    = id_make "puti_var";
          entry_scope = sco;
          entry_info  = ENTRY_parameter inf_p
        } in
        ([p], Some(Null)) )
    | "putc" -> (
        let inf_p = {
          parameter_type = TY_char;
          parameter_offset = 0;
          parameter_mode = PASS_BY_VALUE;
          parameter_value = None
        } in
        let p = {
          entry_id    = id_make "putc_var";
          entry_scope = sco;
          entry_info  = ENTRY_parameter inf_p
        } in
        ([p], Some(Null)) )
    | "putb" -> (
        let inf_p = {
          parameter_type = TY_bool;
          parameter_offset = 0;
          parameter_mode = PASS_BY_VALUE;
          parameter_value = None
        } in
        let p = {
          entry_id    = id_make "putb_var";
          entry_scope = sco;
          entry_info  = ENTRY_parameter inf_p
        } in
        ([p], Some(Null)) )
    | "puts" -> (
        let inf_p = {
          parameter_type = TY_array (TY_char);
          parameter_offset = 0;
          parameter_mode = PASS_BY_VALUE;
          parameter_value = None
        } in
        let p = {
          entry_id    = id_make "puts_var";
          entry_scope = sco;
          entry_info  = ENTRY_parameter inf_p
        } in
        ([p], Some(Null)) )
    | "geti" -> ([], Some(TY_int))
    | "getc" -> ([], Some(TY_char))
    | "getb" -> ([], Some(TY_bool))
    | "gets" ->(
      let inf_p1 = {
        parameter_type = TY_int;
        parameter_offset = 0;
        parameter_mode = PASS_BY_VALUE;
        parameter_value = None
      } in
      let p1 = {
        entry_id    = id_make "gets_var1";
        entry_scope = sco;
        entry_info  = ENTRY_parameter inf_p1
      } in
      let inf_p2 = {
        parameter_type = TY_array (TY_char);
        parameter_offset = 1;
        parameter_mode = PASS_BY_VALUE;
        parameter_value = None
      } in
      let p2 = {
        entry_id    = id_make "gets_var2";
        entry_scope = sco;
        entry_info  = ENTRY_parameter inf_p2
      } in
        ([p1; p2], Some(Null)) )
    | "abs" -> (
        let inf_p = {
          parameter_type = TY_int;
          parameter_offset = 0;
          parameter_mode = PASS_BY_VALUE;
          parameter_value = None
        } in
        let p = {
          entry_id    = id_make "abs_var";
          entry_scope = sco;
          entry_info  = ENTRY_parameter inf_p
        } in
        ([p], Some(TY_int)) )
    | "ord" -> (
        let inf_p = {
          parameter_type = TY_char;
          parameter_offset = 0;
          parameter_mode = PASS_BY_VALUE;
          parameter_value = None
        } in
        let p = {
          entry_id    = id_make "ord_var";
          entry_scope = sco;
          entry_info  = ENTRY_parameter inf_p
        } in
        ([p], Some(TY_int)) )
    | "chr" -> (
        let inf_p = {
          parameter_type = TY_int;
          parameter_offset = 0;
          parameter_mode = PASS_BY_VALUE;
          parameter_value = None
        } in
        let p = {
          entry_id    = id_make "chr_var";
          entry_scope = sco;
          entry_info  = ENTRY_parameter inf_p
        } in
        ([p], Some(TY_char)) )
    | "strlen" -> (
        let inf_p = {
          parameter_type = TY_array (TY_char);
          parameter_offset = 0;
          parameter_mode = PASS_BY_VALUE;
          parameter_value = None
        } in
        let p = {
          entry_id    = id_make "strlen_var";
          entry_scope = sco;
          entry_info  = ENTRY_parameter inf_p
        } in
        ([p], Some(TY_int)) )
    | "strcmp" -> (
        let inf_p = {
          parameter_type = TY_array (TY_char);
          parameter_offset = 0;
          parameter_mode = PASS_BY_VALUE;
          parameter_value = None
        } in
        let p = {
          entry_id    = id_make "strcmp_var";
          entry_scope = sco;
          entry_info  = ENTRY_parameter inf_p
        } in
        ([p; p], Some(TY_int)) )
    | "strcpy" -> (
        let inf_p = {
          parameter_type = TY_array (TY_char);
          parameter_offset = 0;
          parameter_mode = PASS_BY_VALUE;
          parameter_value = None
        } in
        let p = {
          entry_id    = id_make "strcpy_var";
          entry_scope = sco;
          entry_info  = ENTRY_parameter inf_p
        } in
        ([p; p], Some(Null)) )
    | "strcat" -> (
        let inf_p = {
          parameter_type = TY_array (TY_char);
          parameter_offset = 0;
          parameter_mode = PASS_BY_VALUE;
          parameter_value = None
        } in
        let p = {
          entry_id    = id_make "strcat_var";
          entry_scope = sco;
          entry_info  = ENTRY_parameter inf_p
        } in
        ([p; p], Some(Null)) )
    | "GC_init" -> ([], Some(Null))
    | "GC_malloc" -> (
        let inf_p = {
          parameter_type = TY_int;
          parameter_offset = 0;
          parameter_mode = PASS_BY_VALUE;
          parameter_value = None
        } in
        let p = {
          entry_id    = id_make "gc_malloc_var";
          entry_scope = sco;
          entry_info  = ENTRY_parameter inf_p
        } in
        ([p], Some(TY_array(TY_char))) )
    | _ -> raise UnknownError
  ) in
  let f = {
    function_isForward = false;
    function_paramlist = par;
    function_redeflist = [];
    function_result = res;
    function_pstatus = PARDEF_DEFINE;
    function_initquad = 0;
    function_llvalue = None;
    outer_scope_list = get_current_vars_list ();
  } in
  ignore (newEntry id (ENTRY_function f) false)


let invalid_log_op t1 t2 =
  match t1, t2 with
  | (TY_int, TY_int) | (TY_char, TY_char) | (TY_bool, TY_bool) -> false
  | _ -> true

let sem_formal f formal =
  match formal with
  | Formal(parPas, t, slist) ->
    if slist <> [] then (
      let idlist = List.rev_map id_make slist in
      let par_pas = (
        match parPas with
        | BY_val -> PASS_BY_VALUE
        | BY_ref -> PASS_BY_REFERENCE
      ) in
      ignore( List.map (newParameter true f par_pas t None) idlist )
    )
    else (
      error "empty list with formals";
      raise UnknownError
    )

let rec sem_func_def ast =
  match ast with
  | Func_def (Header(tOp, nm, formals), defs, stmts) ->
    let id = id_make nm in
    let f = newFunction id true in
    let typ = (
      match tOp with
      | Some t -> t
      | None -> Null
    ) in
    openScope typ nm;
    List.iter (sem_formal f) formals;
    endFunctionHeader f typ None;
    List.iter sem_def defs;
    List.iter sem_stmt stmts;
    closeScope true

and sem_func_decl ast =
  match ast with
  | Func_decl (Header(tOp, nm, formals)) ->
    let id = id_make nm in
    let f = newFunction id true in
    let typ = (
      match tOp with
      | Some(t) -> t
      | None -> Null
    ) in
    openScope typ nm;
    List.iter (sem_formal f) formals;
    forwardFunction f;
    endFunctionHeader f typ None;
    closeScope true


and sem_var_def ast =
  match ast with
  | Var_def(t, s_list)	->
    if s_list <> [] then (
       let idlist = List.rev_map id_make s_list in
       ignore(List.map (newVariable true t None) idlist)
    )
    else (
      error "empty list with variables";
      raise UnknownError
    )


and sem_def ast =
  match ast with
  | F_def fdef   -> sem_func_def fdef
  | F_decl fdecl -> sem_func_decl fdecl
  | V_def vdef   -> sem_var_def vdef

and sem_expr ast =
  (*cannot avoid raising an exception; otherwise, compiler throws error expecting type typ*)
  match ast with
  | E_atom a -> sem_atom a
  | E_int_const n -> TY_int
  | E_char_const c -> TY_char
  | E_un_plus (e, line) ->
    let t = sem_expr e in
    (if t <> TY_int then
       (error "operator '+' expected integer as operand";
        raise (TypeError line))
     else TY_int)
  | E_un_minus (e, line) ->
    let t = sem_expr e in (
      if t <> TY_int then (
        error "operator '-' expected integer as operand";
        raise (TypeError line)
      )
      else TY_int
    )
  | E_op (e1, op, e2, line) ->
    let t1 = sem_expr e1
    and t2 = sem_expr e2 in (
      if t1 <> TY_int || t2 <> TY_int then (
        let opString = op_as_string op in
        error "operator '%s' expected integers as operands" opString;
        raise (TypeError line)
       )
      else TY_int
    )
  | E_lg_op (e1, op, e2, line) ->
    let t1 = sem_expr e1
    and t2 = sem_expr e2 in (
      if invalid_log_op t1 t2 then (
        let opString = lg_as_string op in
        error "operator '%s' expected operands of same primitive type" opString;
        raise (TypeError line)
      )
      else TY_bool
    )
  | E_bool b -> TY_bool
  | E_not (e, line) ->
    let t = sem_expr e in (
      if t <> TY_bool then (
        error "operator 'not' expected boolean as operand";
        raise (TypeError line)
      )
      else TY_bool
    )
  | E_and_or (e1, ao, e2, line) ->
    let t1 = sem_expr e1
    and t2 = sem_expr e2 in (
      if t1 <> TY_bool || t2 <> TY_bool then (
        let opString = andor_as_string ao in
        error "operator '%s' expected booleans as operands" opString;
        raise (TypeError line)
      )
      else TY_bool
    )
  | E_new (a, e, line) ->
    let t = sem_expr e in (
      if not (a <> Null) || t <> TY_int then (
        error "operator 'new' expected a valid type and an integer as operands";
        raise (TypeError line)
      )
      else TY_array a
    )
  | E_nil -> TY_list Null
  | E_is_nil (e, line) ->
    let t = sem_expr e in (
      match t with
      | TY_list(_) -> TY_bool
      | _ -> (
         error "operator 'nil?' expected operand of type list";
         raise (TypeError line)
        )
    )
  | E_cons (e1, e2, line) ->
    let v1 = sem_expr e1
    and v2 = sem_expr e2 in (
      match v2 with
      | TY_list(v) ->
       if not(equal_typs v v1) && v <> Null then (
         error "type of list of right operand of operator '#' \
                 must be either of same type as left operand or empty list";
         raise (TypeError line)
       )
       else (
         if v == Null then TY_list(v1)
         else v2
       )
      | _          -> (
         error "operator '#' expected a valid type and a list as operands";
         raise (TypeError line)
       )
    )
  | E_head (e, line) ->
    let v = sem_expr e in (
      match v with
      | TY_list Null -> (
          error "Cannot run operator 'head' with nil.";
          raise (TypeError line)
        )
      | TY_list(l) -> l
      | _ -> (
          error "operator 'head' expected operand of type list";
          raise (TypeError line)
        )
    )
  | E_tail (e, line) ->
    let v = sem_expr e in (
      match v with
      | TY_list Null -> (
          error "Cannot run operator 'tail' with nil.";
          raise (TypeError line)
        )
      | TY_list(l) -> v
      | _          -> (
         error "operator 'tail' expected operand of non-empty list type";
         raise (TypeError line)
      )
    )

and check_param exs pars fname line =
  (*parameters by reference have to be lvalues; we do not check this!*)
  match exs, pars with
  | ([], [])       -> ()
  | (e::te, p::tp) -> (
    match p.entry_info with
    | ENTRY_parameter(pi) ->
      let t = sem_expr e in
        if not(equal_typs pi.parameter_type t) then (
          error "parameter type in call of function '%s' inconsistent with type in function definition" fname;
          raise (TypeError line)
        )
        else (
          match pi.parameter_mode, e with
          | ( PASS_BY_REFERENCE, E_atom (_) ) -> ()
          | ( PASS_BY_REFERENCE, _ ) -> (
              error "parameter passed by reference has to be an lvalue in call of function '%s'" fname;
              raise (TypeError line)
            )
          | _ -> check_param te tp fname line
        )
    | _  -> raise UnknownError
    )
  | _ -> (
      error "fewer or more parameters than expected in call of function '%s'" fname;
      raise (TypeError line)
    )

and sem_call c =
  match c with
  | C_call(nm, exprs, line) ->
    let id = id_make nm in
    let e = lookupEntry id LOOKUP_ALL_SCOPES true in (
      match e.entry_info with
      | ENTRY_function(f) ->
         begin
           check_param exprs f.function_paramlist nm line;
           match f.function_result with
            | Some (t) -> t
            | None -> (
                error "Function type doesn't exist";
                raise (TypeError line)
              )
         end
      | _ -> (
          error "identifier '%s' is not a function" nm;
          raise (TypeError line)
        )
    )

and sem_atom ast =
  (*must check: -the atoms are well-defined
                -return the correct typ for each atom*)
  match ast with
  | A_var (v, line) ->
    let id = id_make v in
    let e = lookupEntry id LOOKUP_ALL_SCOPES true in (
      match e.entry_info with
      | ENTRY_variable(v)  -> v.variable_type
      | ENTRY_parameter(v) -> v.parameter_type
      | _                  -> (
        error "identifier '%s' is neither variable nor parameter" v;
        raise (TypeError line)
      )
    )
  | A_string_const str -> TY_array TY_char
  | A_atom (a, e, line) ->
    let v = sem_atom a
    and n = sem_expr e in (
      match v, n with
      | TY_array(t), TY_int -> t
      | _, TY_int -> (
          error "identifier is not an array";
          raise (TypeError line)
        )
      | _ -> (
         error "array index not int";
         raise (TypeError line)
       )
    )
  | A_call c -> sem_call c

and sem_simple ast =
  match ast with
  | S_skip () -> ()
  | S_assign (A_string_const _, _, line) -> (
      error "lvalue in assingment cannot be of type const string";
      raise (TypeError line)
    )
  | S_assign (a, e, line) ->
    let x = sem_atom a
    and y = sem_expr e in
    if not(equal_typs x y) then (
      error "lvalue and rvalue in assignment are not of the same type";
      raise (TypeError line)
    )
  | S_call c ->
    let v = sem_call c	in
    if v <> Null then raise UnknownError

and sem_stmt ast =
  match ast with
  | S_simple s -> sem_simple s
  | S_exit line ->
    let rv = get_cur_return_value () in
    if rv <> Null then
      (error "exit command can only be used in void functions";
       raise (TypeError line))
    else ()
  | S_return (e, line) ->
    let t = sem_expr e
    and rv = get_cur_return_value () in
    if not(equal_typs rv t) then (
      error "type of object to be returned incompatible with return type of function";
      raise (TypeError line)
    )
    else ()
  | S_if (e, stmts, elsif, els, line) ->
    let var = sem_expr e in (
      match var with
      | TY_bool -> List.iter sem_stmt stmts;
                   (match elsif with
                    | Some (elif) -> sem_elsif_stmt elif els
                    | None			  ->
                      (match els with
                        | Some(es) -> sem_else_stmt es
                        | None		 -> () ))
      | _       ->
        error "condition in if-statement must be evaluated as boolean";
        raise (TypeError line)
    )
  | S_for (simples1, e, simples2, stmts, line) ->
    List.iter sem_simple simples1;
    let var = sem_expr e in (
      match var with
      | TY_bool ->
        List.iter sem_simple simples2;
        List.iter sem_stmt stmts
      | _       ->
        error "condition in for-statement must be evaluated as boolean";
        raise (TypeError line)
    )

and sem_elsif_stmt ast els =
  match ast with
  | S_elsif (e, stmts, elsif, line) ->
    let var = sem_expr e in (
      match var with
      | TY_bool	-> List.iter sem_stmt stmts;
                   (match elsif with
                    | Some (elif) -> sem_elsif_stmt elif els
                    | None        ->
                      (match els with
                        | Some(es) -> sem_else_stmt es
                        | None		 -> () ))
      | _ ->
        error "condition in elsif-statement must be evaluated as boolean";
        raise (TypeError line)
    )

and sem_else_stmt ast =
  match ast with
  | S_else stmts -> List.iter sem_stmt stmts

and sem_func ast =
  match ast with
  | Func_def (Header(None, _, []), defs, stmts) ->
    let function_list = ["puti"; "putb"; "puts"; "putc"; "geti"; "getb"; "gets"; "getc"; "abs"; "ord"; "chr"; "strlen"; "strcmp"; "strcpy"; "strcat"; "GC_init"; "GC_malloc"] in
    List.iter insert_built_in_function function_list;
    openScope Null "main";
    List.iter sem_def defs;
    List.iter sem_stmt stmts;
    closeScope true
  | _ ->
    error "main function must be void and without arguments";
    raise (TypeError 1)

let sem ast = sem_func ast

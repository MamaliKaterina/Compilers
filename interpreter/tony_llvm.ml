open Identifier
open Error
open Helping_types
open Tony_symbol

exception UnknownError (*use this when the error is unexpected and cannot understand why came up*)
exception TypeError of int

type llvm_info = {
  context          : Llvm.llcontext;
  the_module       : Llvm.llmodule;
  builder          : Llvm.llbuilder;
  i1               : Llvm.lltype;
  i8               : Llvm.lltype;
  i32              : Llvm.lltype;
  i64              : Llvm.lltype;
  void             : Llvm.lltype;
  tony_list        : Llvm.lltype;
  c1               : int -> Llvm.llvalue;
  c8               : int -> Llvm.llvalue;
  c32              : int -> Llvm.llvalue;
  c64              : int -> Llvm.llvalue;
  make_string      : string -> Llvm.llvalue;
  global_counter   : int ref;
  (*the_vars         :Llvm.llvalue;
  the_nl           : Llvm.llvalue;
  the_writeInteger : Llvm.llvalue;
    the_writeString  : Llvm.llvalue; *)
}

(* Helping function that returns whether the types of two parameters t1, t2
   are valid for a logical operation: they must be of the same type which
   mitgh be int, bool, char.
   Actually, we return the opposite, whether t1, t2 is NOT valid, because it
   helps with the code below. *)
let invalid_log_op info t1 t2 =
  let a = Llvm.type_of t1 in
  let b = Llvm.type_of t2 in
  let i32 = info.i32 in
  let i8 = info.i8 in
  let i1 = info.i1 in
  if a = b then
  (match a with
    | i32 -> false
    | i8  -> false
    | i1  -> false
    | _   -> true )
  else true

(* Helping function that gives the Llvm lltype from "our" typ *)
let rec compile_type info t =
  match t with
  | Null         -> error "unexpected Null type";
                    raise UnknownError
  | TY_int       -> info.i32
  | TY_bool      -> info.i1
  | TY_char      -> info.i8
  | TY_array (p) -> let t = compile_type info p in
                    Llvm.pointer_type t
  | TY_list (p)  -> let t = compile_type info p in
                    Llvm.element_type (Llvm.struct_type info.context (Array.of_list([t; (Llvm.element_type info.tony_list)])))


let compile_formal info f fu cntr formal =
  match formal with
  | Formal(parPas, t, slist) -> let ty = compile_type info t in
                                (match parPas with
                                 | BY_val -> let callnewParam id =
                                              let new_id = id_make id in
                                              let par_value = Llvm.build_malloc ty id info.builder in
                                              Llvm.set_value_name id (Llvm.param fu !cntr);
                                              ignore (Llvm.build_store (Llvm.param fu !cntr) par_value info.builder);
                                              cntr := !cntr + 1;
                                              ignore (newParameter true f PASS_BY_VALUE t new_id par_value)
                                            in List.iter callnewParam slist
                                (*in ref vars do we need to use pointers? if we dont do anything, how will it behave according to calling convention*)
                                 | BY_ref -> let callnewParam id =
                                              let new_id = id_make id in
                                              Llvm.set_value_name id (Llvm.param fu !cntr);
                                              let par_value = Llvm.param fu !cntr in
                                              ignore (newParameter true f PASS_BY_REFERENCE t new_id par_value);
                                              cntr := !cntr + 1
                                            in List.iter callnewParam slist )

let sem_formal f fu cntr formal =
  match formal with
  | Formal(parPas, t, slist) -> let idlist = List.map id_make slist in
                                if idlist <> [] then (
                                  let par_pas = (match parPas with
                                                 | BY_val -> PASS_BY_VALUE
                                                 | BY_ref -> PASS_BY_REFERENCE ) in
                                  let callnewParam id =
                                      ignore (newParameter true f par_pas t id (Llvm.param fu !cntr));
                                      cntr := !cntr + 1
                                  in List.iter callnewParam idlist )
                                else (error "empty list with formals";
                                      raise UnknownError)

let rec params_type info res frm =
  match frm with
  | [] -> List.rev res
  | hd::tl -> (match hd with
               | Formal (parPas, t, nms) -> let ty = compile_type info t in
                                            let ty = (if parPas = BY_ref then (Llvm.pointer_type ty)
                                                      else ty ) in (*what if an array is passed by ref?*)
                 let new_res = List.rev_append (List.rev_map (fun _ -> ty) nms) res in
                 params_type info new_res tl)

(*let set_names frms params =
  match (frms, params) with
  | ([], []) -> ()
  | (hd1::tl1, p) -> (match hd1 with
      | Formal (_, _, nms) -> let set_names_helper l1 l2 =
                                (match (l1, l2) with
                                 | (hd1::tl1, hd2::tl2) -> Llvm.set_value_name hd1 hd2 (*maybe id = id_make hd1, id<->hd1!!!*)
                                 | ([], l)              -> l ) in
                              let rest = set_names_helper nms p in
                              set_names tl1 rest )
  | _ -> error "formals and parametres of function not equal"
*)

let rec compile_func_def info ast =
  match ast with
  | Func_def (Header(tOp, nm, formals), defs, stmts) -> let cur_bb = Llvm.insertion_block info.builder in
                                                        let cur_f = Llvm.block_parent cur_bb in
                                                        let next_bb = Llvm.append_block info.context "return" cur_f in
                                                        ignore (Llvm.build_br next_bb info.builder);
                                                        let id = id_make nm in
                                                        let f = newFunction id true in
                                                        let ret_t = (match tOp with
                                                                     | Some (t) -> compile_type info t
                                                                     | None     -> info.void ) in
                                                        let par_ar = Array.of_list (params_type info [] formals) in
                                                        let ty = Llvm.function_type ret_t par_ar in
                                                        let fu = Llvm.declare_function nm ty info.the_module in
                                                        let bb = Llvm.append_block info.context "entry" fu in
                                                        Llvm.position_at_end bb info.builder;
                                                        let cntr = ref 0 in
                                                        openScope ret_t;
                                                        List.iter (compile_formal info f fu cntr) formals;
                                                        endFunctionHeader f fu ret_t;
                                                        List.iter (compile_def info) defs;
                                                        List.iter (compile_stmt info) stmts;
                                                        ignore (Llvm.build_ret_void info.builder);
                                                        Llvm.position_at_end next_bb info.builder;
                                                          (*Llvm.position_at_end ret_b info.builder;...???*)
                                                        closeScope ()

and compile_func_decl info ast =
  (*must keep the fact that the function must be properly defined!!!*)
  match ast with
  | Func_decl (Header(tOp, nm, formals)) -> let id = id_make nm in
                                            let f = newFunction id true in
                                            let ret_t = (match tOp with
                                                         | Some(t) -> compile_type info t
                                                         | None    -> info.void ) in
                                            let par_ar = Array.of_list (params_type info [] formals) in
                                            let ty = Llvm.function_type ret_t par_ar in
                                            let fu = Llvm.declare_function nm ty info.the_module in
                                            let cntr =  ref 0 in
                                            List.iter (sem_formal f fu cntr) formals;
                                            forwardFunction f;
                                            endFunctionHeader f fu ret_t

and compile_var_def info ast =
  match ast with
  | Var_def(t, s_list)	-> if s_list <> [] then (
                             let ty = compile_type info t in
                             let callnewParam id = (
                               let new_id = id_make id in
                               let v = Llvm.build_malloc ty id info.builder in
                               ignore (newVariable true t new_id v) )
                             in List.iter callnewParam s_list
                           )
                           else (error "empty list with variables";
                                 raise UnknownError)

and compile_def info ast =
  match ast with
  | F_def fdef   -> compile_func_def info fdef
  | F_decl fdecl -> compile_func_decl info fdecl
  | V_def vdef   -> compile_var_def info vdef

and compile_expr info ast =
  (*cannot avoid raising an exception; otherwise, compiler throws error expecting type typ*)
  match ast with
  | E_atom a                    -> let p = compile_atom info a in
                                   Llvm.build_load p "loadtmp" info.builder
  | E_int_const n               -> info.c32 n (*64-bit integers?*)
  | E_char_const c              -> info.c8 (Char.code c)
  | E_un_plus (e, line)         -> let t = compile_expr info e in
                                   (if (Llvm.type_of t) <> info.i32 then (error "operator '+' expected integer as operand";
                                                                          raise (TypeError line))
                                   else t)
  | E_un_minus (e, line)        -> let t = compile_expr info e in
                                   (if (Llvm.type_of t) <> info.i32 then (error "operator '-' expected integer as operand";
                                                                          raise (TypeError line))
                                    else Llvm.build_neg t "negtmp" info.builder )
  | E_op (e1, op, e2, line)     -> let t1 = compile_expr info e1
                                   and t2 = compile_expr info e2 in
                                   (if (Llvm.type_of t1) <> info.i32 || (Llvm.type_of t2) <> info.i32 then
                                      (let opString = op_as_string op in
                                       error "operator '%s' expected integers as operands" opString;
                                       raise (TypeError line))
                                    else (match op with
                                         | O_plus  -> Llvm.build_add t1 t2 "addtmp" info.builder (*returns the llvalue*)
                                         | O_minus -> Llvm.build_sub t1 t2 "subtmp" info.builder
                                         | O_times -> Llvm.build_mul t1 t2 "multmp" info.builder
                                         | O_div   -> Llvm.build_sdiv t1 t2 "divtmp" info.builder
                                         | O_mod   -> Llvm.build_srem t1 t2 "modtmp" info.builder ) )
  | E_lg_op (e1, op, e2, line)  -> let t1 = compile_expr info e1
                                   and t2 = compile_expr info e2 in
                                   (if invalid_log_op info t1 t2 then
                                      (let opString = lg_as_string op in
                                       error "operator '%s' expected operands of same primitive type" opString;
                                       raise (TypeError line))
                                    else (match op with
                                         | LO_eq 			      -> Llvm.build_icmp Llvm.Icmp.Eq t1 t2 "eqtmp" info.builder
                                         | LO_dif			      -> Llvm.build_icmp Llvm.Icmp.Ne t1 t2 "diftmp" info.builder
                                         | LO_less			    -> Llvm.build_icmp Llvm.Icmp.Slt t1 t2 "eqtmp" info.builder
                                         | LO_greater 		  -> Llvm.build_icmp Llvm.Icmp.Sgt t1 t2 "eqtmp" info.builder
                                         | LO_less_eq 		  -> Llvm.build_icmp Llvm.Icmp.Sle t1 t2 "eqtmp" info.builder
                                         | LO_greater_eq 	  -> Llvm.build_icmp Llvm.Icmp.Sge t1 t2 "eqtmp" info.builder ) )
  | E_bool b                    -> info.c1 (if b = True then 1 else 0)
  | E_not (e, line)             -> let t = compile_expr info e in
                                   (if (Llvm.type_of t) <> info.i1 then (error "operator 'not' expected boolean as operand";
                                                                         raise (TypeError line))
                                    else Llvm.build_not t "nottmp" info.builder )
  | E_and_or (e1, ao, e2, line) -> let t1 = compile_expr info e1
                                   and t2 = compile_expr info e2 in
                                   (if (Llvm.type_of t1) <> info.i1 || (Llvm.type_of t2) <> info.i1 then
                                      (let opString = andor_as_string ao in
                                       error "operator '%s' expected booleans as operands" opString;
                                       raise (TypeError line))
                                    else (match ao with
                                         | And -> Llvm.build_and t1 t2 "andtmp" info.builder
                                         | Or  -> Llvm.build_or t1 t2 "ortmp" info.builder ) )
  | E_new (a, e, line)          -> let t = compile_expr info e in
                                   (if not (a <> Null) || (Llvm.type_of t) <> info.i32 then (error "operator 'new' expected a valid type and an integer as operands";
                                                                                            raise (TypeError line))
                                    else (Llvm.build_array_malloc (compile_type info a) t "arraytmp" info.builder ) )
  | E_nil                       -> Llvm.const_pointer_null info.tony_list
  | E_is_nil (e, line)          -> (let t = compile_expr info e in
                                   try
                                     ignore (Llvm.struct_name (Llvm.element_type (Llvm.type_of t)));
                                     Llvm.build_is_null t "nulltmp" info.builder
                                   with
                                   | _ -> error "operator 'nil?' expected operand of type list";
                                     raise (TypeError line))
  | E_cons (e1, e2, line)       -> (let v1 = compile_expr info e1
                                   and v2 = compile_expr info e2 in
                                       try
                                         let struct_t = Llvm.element_type (Llvm.type_of v2) in
                                         ignore (Llvm.struct_name struct_t);
                                         let el_t = Array.get (Llvm.struct_element_types struct_t) 0 in
                                             if el_t <> (Llvm.type_of v1) then (error "type of list of right operand of operator '#' \
                                                                                                      must be of same type as left operand";
                                                                                               raise (TypeError line))
                                             else (
                                               let l_node = Llvm.build_malloc struct_t "list_node" info.builder in
                                               let elem1 = Llvm.build_gep l_node [| (info.c32 0) |] "elem_1" info.builder in
                                               let elem2 = Llvm.build_gep l_node [| (info.c32 1) |] "elem_2" info.builder in
                                               let ptr_change = Llvm.build_bitcast v2 info.tony_list "tmpbitcast" info.builder in
                                               ignore (Llvm.build_store v1 elem1 info.builder);
                                               ignore (Llvm.build_store ptr_change elem2 info.builder);
                                               l_node
                                             )
                                   with
                                   | _ -> error "operator '#' expected a valid type and a list as operands";
                                     raise (TypeError line) )
  | E_head (e, line)            -> (let v = compile_expr info e in
                                   try
                                     ignore (Llvm.struct_name (Llvm.type_of v));
                                     let ptr = Llvm.build_gep v [| (info.c64 0); (info.c32 0) |] "head_ptr" info.builder in
                                     Llvm.build_load ptr "head" info.builder
                                   with
                                   | _ -> error "operator 'head' expected operand of type list";
                                     raise (TypeError line) )
  | E_tail (e, line)            -> (let v = compile_expr info e in
                                   try
                                     ignore (Llvm.struct_name (Llvm.type_of v));
                                     let ptr = Llvm.build_gep v [| (info.c64 0); (info.c32 1) |] "tail_ptr" info.builder in
                                     let tl = Llvm.build_load ptr "tail" info.builder in
                                     Llvm.build_bitcast tl (Llvm.type_of v) "tmpbitcast" info.builder
                                   with
                                   | _ ->  error "operator 'tail' expected operand of type list";
                                     raise (TypeError line))


and check_param fname line info exp par =
    match par.entry_info with
      | ENTRY_parameter(pi) ->
        let arg = (
          if pi.parameter_mode <> PASS_BY_VALUE then (
            match exp with
            | E_atom (a) ->
               compile_atom info a
            | _          -> (error "not l-value as pass by reference argument";
                             raise (TypeError line))
          )
         else (
           compile_expr info exp )
       ) in
       if (Llvm.type_of arg) <> (Llvm.type_of pi.parameter_value) then
         (error "parameter type in call of function '%s' inconsistent with type in function definition" fname;
          raise (TypeError line))
       else arg
     | _ ->
       raise UnknownError

and compile_call info c =
  match c with
  | C_call(nm, exprs, line) ->
    let id = id_make nm in
    let e = lookupEntry id LOOKUP_ALL_SCOPES true in
    (match e.entry_info with
    | ENTRY_function(f) ->
      if not (f.function_isForward) then
        begin
          try
            let params_array = Array.of_list (List.map2 (check_param nm line info) exprs f.function_paramlist) in (*???*)
            Llvm.build_call (match f.function_llvalue with Some (v) -> v) params_array "calltmp" info.builder
          with Invalid_argument _ ->
            error "fewer or more parameters than expected in call of function '%s'" nm;
            raise (TypeError line)
        end
      else (
        error "function '%s' in not yet defined" nm;
        raise (TypeError line)
      )
    | _ ->
      error "identifier '%s' in not a function" nm;
      raise (TypeError line) )

and compile_atom info ast =
  match ast with
  | A_var (v, line)     -> let id = id_make v in
                           let e = lookupEntry id LOOKUP_ALL_SCOPES true in
                           (match e.entry_info with
                            | ENTRY_variable(v)  -> v.variable_value
                            | ENTRY_parameter(v) -> v.parameter_value
                            | ENTRY_temporary(v) -> v.temporary_value
                            | _                  -> error "identifier '%s' is neither variable nor parameter" v;
                                                    raise (TypeError line) )
  | A_string_const str  -> let str = info.make_string str in (*when reading the string from input it does not contain the white character in the end?*)
                           (info.global_counter) := !(info.global_counter) + 1;
                           let num = string_of_int (!(info.global_counter)) in
                           let name = (Printf.sprintf "string%s" num) in
                           let ptr_name = (Printf.sprintf "string_ptr%s" num) in
                           let glb = Llvm.define_global name str info.the_module in
                           let first_elem_ptr = Llvm.build_gep glb [|(info.c32 0); (info.c32 0)|] "string" info.builder in
                           Llvm.define_global ptr_name first_elem_ptr info.the_module
  | A_atom (a, e, line) -> let v = compile_atom info a
                           and n = compile_expr info e in
                           begin
                           try
                             ignore (Llvm.element_type (Llvm.element_type (Llvm.type_of v)));
                             if (Llvm.type_of n) <> info.i64 then (error "array index not int"; (*print_typ only for debugging*)
                                                                   raise (TypeError line) )
                             else Llvm.build_gep v [|(info.c32 0); (info.c32 0); n|] "arraytmp" info.builder (*???+inbounds?*)
                           with
                           | _ -> (error "identifier is not an array";
                                   raise (TypeError line))
                           end
  | A_call c            -> compile_call info c

and compile_simple info ast =
  match ast with
  | S_skip ()             -> () (*ignored by compiler? else must find suitable llvm instruction*)
  | S_assign (a, e, line) -> let x = compile_atom info a
                             and y = compile_expr info e in
                             if (Llvm.element_type (Llvm.type_of x)) <> (Llvm.type_of y) then (error "lvalue and rvalue in assignment are not of the same type";
                                                                                               (Printf.eprintf "%s, %s\n" (Llvm.string_of_lltype (Llvm.element_type (Llvm.type_of x))) (Llvm.string_of_lltype (Llvm.type_of y)));
                                                                                raise (TypeError line))
                             else ignore (Llvm.build_store y x info.builder) (*if it is an array assingment i have to assign x the pointer*)
  | S_call c              -> let v = compile_call info c	in
                             if (Llvm.type_of v) <> info.void then raise UnknownError

and compile_stmt info ast =
    match ast with
    | S_simple s                                 -> compile_simple info s
    | S_exit line                                -> let bb = Llvm.insertion_block info.builder in
                                                    let f = Llvm.block_parent bb in
                                                    let f_ty = Llvm.type_of f in
                                                    let ret_ty = Llvm.return_type f_ty in
                                                    if ret_ty <> info.void then (error "type of object to be returned incompatible with return type of function";
                                                                                           raise (TypeError line))
                                                    else ignore (Llvm.build_ret_void info.builder)
    | S_return (e, line)                         -> let t = compile_expr info e in
                                                    let bb = Llvm.insertion_block info.builder in
                                                    let f = Llvm.block_parent bb in
                                                    let f_ty = Llvm.type_of f in
                                                    let ret_ty = Llvm.return_type (Llvm.element_type f_ty) in
                                                    if (Llvm.type_of t) <> ret_ty then (error "type of object to be returned incompatible with return type of function";
                                                                                  (Printf.eprintf "%s, %s\n" (Llvm.string_of_lltype (Llvm.type_of t)) (Llvm.string_of_lltype ret_ty));
                                                                                        raise (TypeError line))
                                                    else ignore (Llvm.build_ret t info.builder)
    | S_if (e, stmts, elsif, els, line)          -> let v = compile_expr info e in
                                                    if (Llvm.type_of v) <> info.i1 then (error "condition in if-statement must be evaluated as boolean";
                                                                                         raise (TypeError line) )
                                                    else (
                                                      let cond = Llvm.build_icmp Llvm.Icmp.Ne v (info.c64 0) "if_cond" info.builder in
                                                      let bb = Llvm.insertion_block info.builder in
                                                      let f = Llvm.block_parent bb in
                                                      let then_bb = Llvm.append_block info.context "then" f in
                                                      let else_bb = Llvm.append_block info.context "else" f in
                                                      let after_bb = Llvm.append_block info.context "after" f in
                                                      ignore (Llvm.build_cond_br cond then_bb else_bb info.builder);
                                                      Llvm.position_at_end then_bb info.builder;
                                                      List.iter (compile_stmt info) stmts;
                                                      ignore (Llvm.build_br after_bb info.builder);
                                                      compile_elsif_stmt info else_bb after_bb elsif els;
                                                      Llvm.position_at_end after_bb info.builder )
    | S_for (simples1, e, simples2, stmts, line) -> List.iter (compile_simple info) simples1;
                                                    let bb = Llvm.insertion_block info.builder in
                                                    let f = Llvm.block_parent bb in
                                                    let loop_bb = Llvm.append_block info.context "loop" f in
                                                    let body_bb = Llvm.append_block info.context "body" f in
                                                    let after_bb = Llvm.append_block info.context "after" f in
                                                    ignore (Llvm.build_br loop_bb info.builder);
                                                    Llvm.position_at_end loop_bb info.builder;
                                                    let n = compile_expr info e in
                                                    if (Llvm.type_of n) <> info.i1 then ( error "condition in for-statement must be evaluated as boolean";
                                                                                          raise (TypeError line) )
                                                    else (
                                                    (*let phi_iter = Llvm.build_phi [(n, bb)] "iter" info.builder in *)
                                                      let loop_cond = Llvm.build_icmp Llvm.Icmp.Ne n (info.c64 0) "loop_cond" info.builder in
                                                      ignore (Llvm.build_cond_br loop_cond body_bb after_bb info.builder);
                                                      Llvm.position_at_end body_bb info.builder;
                                                    (*let remaining = Llvm.build_sub phi_iter (info.c64 1) "remaining" info.builder in*)
                                                      List.iter (compile_stmt info) stmts;
                                                      List.iter (compile_simple info) simples2;
                                                    (*Llvm.add_incoming (remaining, Llvm.insertion_block info.builder) phi_iter;*)
                                                      ignore (Llvm.build_br loop_bb info.builder);
                                                      Llvm.position_at_end after_bb info.builder )

and compile_elsif_stmt info then_bb after_bb ast els =
  match ast with
  | Some( S_elsif (e, stmts, elsif, line) ) -> let v = compile_expr info e in
                                               if (Llvm.type_of v) <> info.i1 then (error "condition in elsif-statement must be evaluated as boolean";
                                                                                    raise (TypeError line) )
                                               else (
                                                 let cond = Llvm.build_icmp Llvm.Icmp.Ne v (info.c64 0) "if_cond" info.builder in
                                                 let bb = Llvm.insertion_block info.builder in
                                                 let f = Llvm.block_parent bb in
                                                 let else_bb = Llvm.append_block info.context "else" f in
                                                 Llvm.position_at_end then_bb info.builder;
                                                 ignore (Llvm.build_cond_br cond then_bb else_bb info.builder);
                                                 List.iter (compile_stmt info) stmts;
                                                 ignore (Llvm.build_br after_bb info.builder);
                                                 compile_elsif_stmt info else_bb after_bb elsif els )
  | None                                    -> compile_else_stmt info then_bb after_bb els

and compile_else_stmt info then_bb after_bb ast =
  match ast with
  | Some ( S_else (stmts) ) -> Llvm.position_at_end then_bb info.builder;
                               List.iter (compile_stmt info) stmts;
                               ignore (Llvm.build_br after_bb info.builder)
  | None                    -> Llvm.position_at_end then_bb info.builder;
                               ignore (Llvm.build_br after_bb info.builder)

and compile_func ast =
  match ast with
  | Func_def(Header(None, nm, []), defs, stmts) ->
    begin
    (* Initialize *)
    Llvm_all_backends.initialize ();
    let context = Llvm.global_context () in
    let the_module = Llvm.create_module context "tony program" in
    let builder = Llvm.builder context in
    let pm = Llvm.PassManager.create () in
    List.iter (fun f -> f pm) [
      Llvm_scalar_opts.add_memory_to_register_promotion;
      Llvm_scalar_opts.add_instruction_combination;
      Llvm_scalar_opts.add_reassociation;
      Llvm_scalar_opts.add_gvn;
      Llvm_scalar_opts.add_cfg_simplification;
    ];
    (* Initialize types, returns lltype *)
    let i1 = Llvm.i1_type context in
    let i8 = Llvm.i8_type context in
    let i32 = Llvm.i32_type context in
    let i64 = Llvm.i64_type context in
    let void = Llvm.void_type context in
    let tony_list = Llvm.named_struct_type context "tony_list" in
    Llvm.struct_set_body tony_list (Array.of_list ([Llvm.element_type i8; i64; (Llvm.element_type tony_list)])) true;
    (* Initialize constant functions, returns llvalue *)
    let c1 = Llvm.const_int i1 in
    let c8 = Llvm.const_int i8 in
    let c32 = Llvm.const_int i32 in
    let c64 = Llvm.const_int i64 in
    let make_string = Llvm.const_stringz context in
    let global_counter = ref 0 in
    (* Initialize global variables *)
    (*let vars_type = Llvm.array_type i64 26 in
    let the_vars = Llvm.declare_global vars_type "vars" the_module in
    Llvm.set_linkage Llvm.Linkage.Private the_vars;
    Llvm.set_initializer (Llvm.const_null vars_type) the_vars;
    Llvm.set_alignment 16 the_vars;
    let nl = "\n" in
    let nl_type = Llvm.array_type i8 (1 + String.length nl) in
    let the_nl = Llvm.declare_global nl_type "nl" the_module in
    Llvm.set_linkage Llvm.Linkage.Private the_nl;
    Llvm.set_global_constant true the_nl;
    Llvm.set_initializer (Llvm.const_stringz context nl) the_nl;
    Llvm.set_alignment 1 the_nl;
    (* Initialize library functions *)
    let writeInteger_type =
      Llvm.function_type (Llvm.void_type context) [| i64 |] in
    let the_writeInteger =
      Llvm.declare_function "writeInteger" writeInteger_type the_module in
    let writeString_type =
      Llvm.function_type (Llvm.void_type context) [| Llvm.element_type i8 |] in
    let the_writeString =
      Llvm.declare_function "writeString" writeString_type the_module in*)
    (* Define and start and main function *)
    let main_type = Llvm.function_type i32 [| |] in
    let main = Llvm.declare_function nm main_type the_module in
    let bb = Llvm.append_block context "entry" main in
    Llvm.position_at_end bb builder;
    (* Emit the program code *)
    let info = {
      context          = context;
      the_module       = the_module;
      builder          = builder;
      i1               = i1;
      i8               = i8;
      i32              = i32;
      i64              = i64;
      void             = void;
      tony_list        = tony_list;
      c1               = c1;
      c8               = c8;
      c32              = c32;
      c64              = c64;
      make_string      = make_string;
      global_counter   = global_counter;
    } in
    List.iter (compile_def info) defs;
    List.iter (compile_stmt info) stmts;
    ignore (Llvm.build_ret (c32 0) builder);
    (* Verify *)
    Llvm_analysis.assert_valid_module the_module;
    (* Optimize
    ignore (Llvm.PassManager.run_module the_module pm); *)
    (* Print out the IR *)
    Llvm.print_module "a.ll" the_module
    end
  | _ -> error "main function must be void and without arguments"

let compile ast = compile_func ast

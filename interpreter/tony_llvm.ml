open Identifier
open Error
open Helping_types
open Tony_symbol
open Llvm

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
  c1               : int -> Llvm.llvalue;
  c8               : int -> Llvm.llvalue;
  c32              : int -> Llvm.llvalue;
  c64              : int -> Llvm.llvalue;
  make_string      : string -> Llvm.llvalue;
  the_vars         : Llvm.llvalue;
  the_nl           : Llvm.llvalue;
  the_writeInteger : Llvm.llvalue;
  the_writeString  : Llvm.llvalue;
}


let context = global_context ()
let the_module = create_module context "compiler"
let builder = builder context
let make_int n = const_int i64_type context n
let make_char c = const_int i8_type context c (*const_char doesnt exist???*)
let make_bool b = const_int i1_type context (to_int b) (*bools as ints???*)
let make_string str = const_string context str


let invalid_log_op t1 t2 =
  match t1, t2 with
  | (TY_int, TY_int) | (TY_char, TY_char) | (TY_bool, TY_bool) -> false
  | _ -> true

let compile_formal info f formal =
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

let rec compile_func_def info ast =
  match ast with
  | Func_def (Header(tOp, nm, formals), defs, stmts) -> let id = id_make nm in
                                                        let f = newFunction id true in
                                                        (match tOp with
                                                         | Some(t) -> openScope t;
                                                                      List.iter (compile_formal f) formals;
                                                                      endFunctionHeader f t
                                                         | None    -> openScope Null;
                                                                      List.iter (compile_formal f) formals;
                                                                      endFunctionHeader f Null );
                                                          (*maybe announce parameters as variables here*)
                                                          List.iter compile_def defs;
                                                          List.iter compile_stmt stmts;
                                                          closeScope ()

and compile_func_decl info ast =
  (*must keep the fact that the function must be properly defined!!!*)
  match ast with
  | Func_decl (Header(tOp, nm, formals)) -> let id = id_make nm in
                                            let f = newFunction id true in
                                              List.iter (compile_formal f) formals;
                                              forwardFunction f;
                                              (match tOp with
                                              | Some(t) -> endFunctionHeader f t
                                              | None    -> endFunctionHeader f Null )

and compile_var_def info ast =
  match ast with
  | Var_def(t, s_list)	-> let idlist = List.rev_map id_make s_list in
                            let rec callnewParam idlist =
                              (match idlist with
                               | [] -> ()
                               | _    -> (match t with
                                          | TY_int -> let v = Llvm.build_malloc info.i64 id info.builder in
                                                      let d = newVariable true t (List.hd idlist) (Some v) in
                                                       callnewParam (List.tl idlist)
                                          | TY_bool -> let v = Llvm.build_malloc info.i1 id info.builder in
                                                       let d = newVariable true t (List.hd idlist) (Some v) in
                                                        callnewParam (List.tl idlist)
                                          | TY_char -> let v = Llvm.build_malloc info.i8 id info.builder in
                                                       let d = newVariable true t (List.hd idlist) (Some v) in
                                                        callnewParam (List.tl idlist)
                                          | TY_array (_) -> let d = newVariable true t (List.hd idlist) None in
                                                              callnewParam (List.tl idlist)
                                          | TY_list (_) -> let d = newVariable true t (List.hd idlist) None in
                                                             callnewParam (List.tl idlist) ) )
                            in callnewParam idlist;

and compile_def info ast =
  match ast with
  | F_def fdef   -> compile_func_def fdef
  | F_decl fdecl -> compile_func_decl fdecl
  | V_def vdef   -> compile_var_def vdef

and compile_expr info ast =
  (*cannot avoid raising an exception; otherwise, compiler throws error expecting type typ*)
  match ast with
  | E_atom a                    -> let p = compile_atom a in
                                   Llvm.build_load p "loadtmp" info.builder
  | E_int_const n               -> info.c64 n (*64-bit integers?*)
  | E_char_const c              -> info.c8 (to_int c) (*???*)
  | E_un_plus (e, line)         -> let t = compile_expr e in
                                   t
  | E_un_minus (e, line)        -> let t = compile_expr e in
                                   Llvm.build_neg t "negtmp" info.builder (*maybe we could avoid that and just return the -e*)
  | E_op (e1, op, e2, line)     -> let t1 = compile_expr e1
                                   and t2 = compile_expr e2 in
                                   (match op with
                                    | O_plus  -> Llvm.build_add t1 t2 "addtmp" info.builder (*returns the llvalue*)
                                    | O_minus -> Llvm.build_sub t1 t2 "subtmp" info.builder
                                    | O_times -> Llvm.build_mul t1 t2 "multmp" info.builder
                                    | O_div   -> Llvm.build_sdiv t1 t2 "divtmp" info.builder
                                    | O_mod   -> Llvm.build_srem t1 t2 "modtmp" info.builder )
  | E_lg_op (e1, op, e2, line)  -> let t1 = compile_expr e1
                                   and t2 = compile_expr e2 in
                                   (*results already of type bool->i1*)
                                   (match op with
                                    | LO_eq 			    -> Llvm.build_icmp Icmp.Eq t1 t2 "eqtmp" info.builder
                                    | LO_dif			    -> Llvm.build_icmp Icmp.Ne t1 t2 "diftmp" info.builder
                                    | LO_less			    -> Llvm.build_icmp Icmp.Slt t1 t2 "eqtmp" info.builder
                                    | LO_greater 		  -> Llvm.build_icmp Icmp.Sgt t1 t2 "eqtmp" info.builder
                                    | LO_less_eq 		  -> Llvm.build_icmp Icmp.Sle t1 t2 "eqtmp" info.builder
                                    | LO_greater_eq 	-> Llvm.build_icmp Icmp.Sge t1 t2 "eqtmp" info.builder )
  | E_bool b                    -> info.c1 (to_int b) (*???*)
  | E_not (e, line)             -> let t = compile_expr e in
                                   Llvm.build_not t "nottmp" info.builder
  | E_and_or (e1, ao, e2, line) -> let t1 = compile_expr e1
                                   and t2 = compile_expr e2 in
                                   (match ao with
                                    | And -> Llvm.build_and t1 t2 "andtmp" info.builder
                                    | Or  -> Llvm.build_or t1 t2 "ortmp" info.builder )
  | E_new (a, e, line)          -> let t = compile_expr e in
                                   (match a with
                                    | TY_int  -> Llvm.build_array_malloc info.i64 t "arraytmp" info.builder
                                    | TY_char -> Llvm.build_array_malloc info.i8 t "arraytmp" info.builder
                                    | TY_bool -> Llvm.build_array_malloc info.i1 t "arraytmp" info.builder )
  | E_nil                       -> Llvm.const_null Llvm.void_type info.context (*??? void_type-> void function ???*)
  | E_is_nil (e, line)          -> let t = compile_expr e in
                                    Llvm.build_is_null t "nulltmp" info.builder
  | E_cons (e1, e2, line)       -> let v1 = compile_expr e1
                                   and v2 = compile_expr e2 in

  | E_head (e, line)            -> let v = compile_expr e in
                                   (match v with
                                    | TY_list(l) -> l
                                    | _          -> error "operator 'head' expected operand of type list";
                                                    raise (TypeError line) )
  | E_tail (e, line)            -> let v = compile_expr e in
                                   (match v with
                                    | TY_list(l) -> v
                                    | _          -> error "operator 'nil?' expected operand of type list";
                                                    raise (TypeError line) )

and check_param info exs pars fname line =
  match exs, pars with
  | ([], [])       -> ()
  | (e::te, p::tp) -> let t = compile_expr e in
                      (match p.entry_info with
                       | ENTRY_parameter(pi) -> if pi.parameter_type <> t then
                                                  (error "parameter type in call of function '%s' inconsistent \
                                                          with type in function definition" fname;
                                                   raise (TypeError line))
                                                else check_param te tp fname line
                       | _                   -> raise UnknownError )
  | _              -> error "fewer or more parameters than expected in call of function '%s'" fname;
                      raise (TypeError line)

and compile_call info c =
  match c with
  | C_call(nm, exprs, line) -> let id = id_make nm in
                               let e = lookupEntry id LOOKUP_ALL_SCOPES true in
                               (match e.entry_info with
                                | ENTRY_function(f) -> if not (f.function_isForward) then
                                                       begin
                                                         check_param exprs f.function_paramlist nm line;
                                                         f.function_result
                                                       end
                                                       else (error "function '%s' in not yet defined" nm;
                                                             raise (TypeError line))
                                | _ -> error "identifier '%s' in not a function" nm;
                                       raise (TypeError line) )

and compile_atom info ast =
  (*must check: -the atoms are well-defined
                -return the correct typ for each atom*)
  match ast with
  | A_var (v, line)     -> let id = id_make v in
                           let e = lookupEntry id LOOKUP_ALL_SCOPES true in
                           (match e.entry_info with
                            | ENTRY_variable(v)  -> v.variable_value
                            | ENTRY_parameter(v) -> v.parameter_type
                            | ENTRY_temporary(v) -> v.temporary_type
                            | _                  -> error "identifier '%s' is neither variable nor parameter" v;
                                                    raise (TypeError line) )
  | A_string_const str  -> info.make_string str
  | A_atom (a, e, line) -> let v = compile_atom a
                           and n = compile_expr e in
                           (match v, n with
                            | (TY_array(t), TY_int) -> t
                            | _                     -> error "identifier is not an array"; print_typ v; (*print_typ only for debugging*)
                                                       raise (TypeError line) )
  | A_call c            -> compile_call c

and compile_simple info ast =
  match ast with
  | S_skip ()             -> ()
  | S_assign (a, e, line) -> let x = compile_atom a
                             and y = compile_expr e in (*it will return the typ of y*)
                             ignore (Llvm.build_store y x info.builder)
  | S_call c              -> let v = compile_call c	in
                             if v <> Null then raise UnknownError

and compile_stmt info ast =
    match ast with
    | S_simple s                                 -> compile_simple info s
    | S_exit line                                -> let rv = get_cur_return_value () in
                                                    if rv <> Null then
                                                      (error "exit command can only be used in void functions";
                                                       raise (TypeError line))
                                                    else ()
    | S_return (e, line)                         -> let t = compile_expr e
                                                    and rv = get_cur_return_value () in
                                                    if rv <> t then
                                                      (error "type of object to be returned incompatible with return type of function";
                                                       raise (TypeError line))
                                                    else ()
    | S_if (e, stmts, elsif, els, line)          -> let v = compile_expr info e in
                                                    let cond = Llvm.build_icmp Llvm.Icmp.Ne v (info.c64 0) "if_cond" info.builder in
                                                    let bb = Llvm.insertion_block info.builder in
                                                    let f = Llvm.block_parent bb in
                                                    let then_bb = Llvm.append_block info.context "then" f in
                                                    let else_bb = Llvm.append_block info.context "else" f in
                                                    let after_bb = Llvm.append_block info.context "after" f in
                                                    ignore (Llvm.build_cond_br cond then_bb else_bb info.builder);
                                                    Llvm.position_at_end then_bb info.builder;
                                                    compile_stmt info stmts;
                                                    ignore (Llvm.build_br after_bb info.builder);
                                                    compile_elsif_stmt info else_bb after_bb elsif els;
                                                    Llvm.position_at_end after_bb info.builder

    | S_for (simples1, e, simples2, stmts, line) -> List.iter (compile_simple info) simples1;
                                                    let bb = Llvm.insertion_block info.builder in
                                                    let f = Llvm.block_parent bb in
                                                    let loop_bb = Llvm.append_block info.context "loop" f in
                                                    let body_bb = Llvm.append_block info.context "body" f in
                                                    let after_bb = Llvm.append_block info.context "after" f in
                                                    ignore (Llvm.build_br loop_bb info.builder);
                                                    Llvm.position_at_end loop_bb info.builder;
                                                    let n = compile_expr info e in
                                                    (*let phi_iter = Llvm.build_phi [(n, bb)] "iter" info.builder in *)
                                                    let loop_cond = Llvm.build_icmp Llvm.Icmp.Ne n (info.c64 0) "loop_cond" info.builder in
                                                    ignore (Llvm.build_cond_br loop_cond body_bb after_bb info.builder);
                                                    Llvm.position_at_end body_bb info.builder;
                                                    (*let remaining = Llvm.build_sub phi_iter (info.c64 1) "remaining" info.builder in*)
                                                    compile_stmt info stmts;
                                                    List.iter (compile_simple info) simples2;
                                                    (*Llvm.add_incoming (remaining, Llvm.insertion_block info.builder) phi_iter;*)
                                                    ignore (Llvm.build_br loop_bb info.builder);
                                                    Llvm.position_at_end after_bb info.builder



and compile_elsif_stmt info then_bb after_bb ast els =
  match ast with
  | Some( S_elsif (e, stmts, elsif, line) ) -> let v = compile_expr info e in
                                               let cond = Llvm.build_icmp Llvm.Icmp.Ne v (info.c64 0) "if_cond" info.builder in
                                               let bb = Llvm.insertion_block info.builder in
                                               let f = Llvm.block_parent bb in
                                               let else_bb = Llvm.append_block info.context "else" f in
                                               Llvm.position_at_end then_bb info.builder;
                                               ignore (Llvm.build_cond_br cond then_bb else_bb info.builder);
                                               compile_stmt info stmts;
                                               ignore (Llvm.build_br after_bb info.builder);
                                               compile_elsif_stmt info else_bb after_bb elsif els
  | None                                    -> compile_else_stmt then_bb after_bb els

and compile_else_stmt info then_bb after_bb ast =
  match ast with
  | Some( S_else (stmts) ) -> Llvm.position_at_end then_bb info.builder;
                              compile_stmt info stmts;
                              ignore (Llvm.build_br after_bb info.builder)
  | None                   -> Llvm.position_at_end then_bb info.builder;
                              ignore (Llvm.build_br after_bb info.builder)


and compile_func info ast =
  match ast with
  | Func_def (Header(None, _, []), defs, stmts) -> List.iter compile_def defs;
                                                   List.iter compile_stmt stmts
  | _ -> error "main function must be void and without arguments"

let compile ast = compile_func ast


let llvm_compile_and_dump asts =
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
  (* Initialize types *)
  let i1 = Llvm.i1_type context in
  let i8 = Llvm.i8_type context in
  let i32 = Llvm.i32_type context in
  let i64 = Llvm.i64_type context in
  (* Initialize constant functions *)
  let c1 = Llvm.const_int i1 in
  let c8 = Llvm.const_int i8 in
  let c32 = Llvm.const_int i32 in
  let c64 = Llvm.const_int i64 in
  let make_string = Llvm.const_string context in
  (* Initialize global variables *)
  let vars_type = Llvm.array_type i64 26 in
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
    Llvm.function_type (Llvm.void_type context) [| Llvm.pointer_type i8 |] in
  let the_writeString =
    Llvm.declare_function "writeString" writeString_type the_module in
  (* Define and start and main function *)
  let main_type = Llvm.function_type i32 [| |] in
  let main = Llvm.declare_function "main" main_type the_module in
  let bb = Llvm.append_block context "entry" main in
  Llvm.position_at_end bb builder;
  (* Emit the program code *)
  let info = {
    context          = context;
    the_module       = the_module;
    builder          = builder;
    i8               = i8;
    i32              = i32;
    i64              = i64;
    c32              = c32;
    c64              = c64;
    the_vars         = the_vars;
    the_nl           = the_nl;
    the_writeInteger = the_writeInteger;
    the_writeString  = the_writeString;
  } in
  List.iter (compile info) asts;
  ignore (Llvm.build_ret (c32 0) builder);
  (* Verify *)
  Llvm_analysis.assert_valid_module the_module;
  (* Optimize *)
  ignore (Llvm.PassManager.run_module the_module pm);
  (* Print out the IR *)
  Llvm.print_module "a.ll" the_module

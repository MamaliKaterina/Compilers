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
  puti             : Llvm.llvalue;
  putb             : Llvm.llvalue;
  puts             : Llvm.llvalue;
  putc             : Llvm.llvalue;
  geti             : Llvm.llvalue;
  getb             : Llvm.llvalue;
  gets             : Llvm.llvalue;
  getc             : Llvm.llvalue;
  abs              : Llvm.llvalue;
  ord              : Llvm.llvalue;
  chr              : Llvm.llvalue;
  strlen           : Llvm.llvalue;
  strcmp           : Llvm.llvalue;
  strcpy           : Llvm.llvalue;
  strcat           : Llvm.llvalue;
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
                    Llvm.pointer_type (Llvm.struct_type info.context (Array.of_list([t; (Llvm.pointer_type info.tony_list)])))

(*Returns true if the two types can be considered equivalent*)
let rec equal_types info x y =
  let x  = (if Llvm.classify_type x = Llvm.TypeKind.Pointer then Llvm.element_type x
            else x) in
  let y  = (if Llvm.classify_type y = Llvm.TypeKind.Pointer then Llvm.element_type y
            else y) in
  if x = y then (true, 0) (*if they are not exactly the same type they still can be equivalent*)
  else (
    if Llvm.classify_type x = Llvm.TypeKind.Struct && y = info.tony_list then (
      let el = Array.get (Llvm.struct_element_types x) 0 in
      if el <> info.tony_list && Llvm.classify_type el <> Llvm.TypeKind.Struct then (true, 2)
      else (false, 0) )
    else (
      if Llvm.classify_type y = Llvm.TypeKind.Struct && x = info.tony_list then (
        let el = Array.get (Llvm.struct_element_types y) 0 in
        if el <> info.tony_list && Llvm.classify_type el <> Llvm.TypeKind.Struct then (true, 1)
        else (false, 0) )
      else (
        if Llvm.classify_type x = Llvm.TypeKind.Struct && Llvm.classify_type y = Llvm.TypeKind.Struct then
          let el1 = Array.get (Llvm.struct_element_types x) 0 in
          let el2 = Array.get (Llvm.struct_element_types y) 0 in
          equal_types info el1 el2
        else (false, 0)
      )
    )
  )



let insert_built_in_function info nm =
  let id =  id_make nm in
  (*create fake scope for parameters*)
  let sco = {
    sco_parent = None;
    sco_nesting = -1;
    sco_entries = [];
    sco_negofs = 0;
    return_value = None
  } in
  let ((par, res), var) = (
    match nm with
    | "puti" -> (
        let inf_p = {
          parameter_type = TY_int;
          parameter_offset = 0;
          parameter_mode = PASS_BY_VALUE;
          parameter_value = Llvm.const_pointer_null (Llvm.pointer_type info.i32);
        } in
        let p = {
          entry_id    = id_make "puti_var";
          entry_scope = sco;
          entry_info  = ENTRY_parameter inf_p
        } in
        (([p], Some(info.void)), Some(info.puti)) )
    | "putc" -> (
        let inf_p = {
          parameter_type = TY_char;
          parameter_offset = 0;
          parameter_mode = PASS_BY_VALUE;
          parameter_value = Llvm.const_pointer_null (Llvm.pointer_type info.i8);
        } in
        let p = {
          entry_id    = id_make "putc_var";
          entry_scope = sco;
          entry_info  = ENTRY_parameter inf_p
        } in
        (([p], Some(info.void)), Some(info.putc)) )
    | "putb" -> (
        let inf_p = {
        parameter_type = TY_bool;
        parameter_offset = 0;
        parameter_mode = PASS_BY_VALUE;
        parameter_value = Llvm.const_pointer_null (Llvm.pointer_type info.i1);
        } in
        let p = {
          entry_id    = id_make "putb_var";
          entry_scope = sco;
          entry_info  = ENTRY_parameter inf_p
        } in
        (([p], Some(info.void)), Some(info.putb)) )
    | "puts" -> (
        let inf_p = {
          parameter_type = TY_array (TY_char);
          parameter_offset = 0;
          parameter_mode = PASS_BY_VALUE;
          parameter_value = Llvm.const_pointer_null (Llvm.pointer_type (Llvm.pointer_type info.i8));
        } in
        let p = {
          entry_id    = id_make "puts_var";
          entry_scope = sco;
          entry_info  = ENTRY_parameter inf_p
        } in
        (([p], Some(info.void)), Some(info.puts)) )
    | "geti" -> (([], Some(info.i32)), Some(info.geti))
    | "getc" -> (([], Some(info.i8)), Some(info.getc))
    | "getb" -> (([], Some(info.i1)), Some(info.getb))
    | "gets" -> (([], Some(Llvm.pointer_type info.i8)), Some(info.gets))
    | "abs" -> (
        let inf_p = {
          parameter_type = TY_int;
          parameter_offset = 0;
          parameter_mode = PASS_BY_VALUE;
          parameter_value = Llvm.const_pointer_null (Llvm.pointer_type info.i32);
        } in
        let p = {
          entry_id    = id_make "abs_var";
          entry_scope = sco;
          entry_info  = ENTRY_parameter inf_p
        } in
        (([p], Some(info.i32)), Some(info.abs)) )
    | "ord" -> (
        let inf_p = {
          parameter_type = TY_char;
          parameter_offset = 0;
          parameter_mode = PASS_BY_VALUE;
          parameter_value = Llvm.const_pointer_null (Llvm.pointer_type info.i8);
        } in
        let p = {
          entry_id    = id_make "ord_var";
          entry_scope = sco;
          entry_info  = ENTRY_parameter inf_p
        } in
        (([p], Some(info.i32)), Some(info.ord)) )
    | "chr" -> (
        let inf_p = {
          parameter_type = TY_int;
          parameter_offset = 0;
          parameter_mode = PASS_BY_VALUE;
          parameter_value = Llvm.const_pointer_null (Llvm.pointer_type info.i32);
        } in
        let p = {
          entry_id    = id_make "chr_var";
          entry_scope = sco;
          entry_info  = ENTRY_parameter inf_p
        } in
        (([p], Some(info.i8)), Some(info.chr)) )
    | "strlen" -> (
        let inf_p = {
          parameter_type = TY_array (TY_char);
          parameter_offset = 0;
          parameter_mode = PASS_BY_VALUE;
          parameter_value = Llvm.const_pointer_null (Llvm.pointer_type (Llvm.pointer_type info.i8));
        } in
        let p = {
          entry_id    = id_make "strlen_var";
          entry_scope = sco;
          entry_info  = ENTRY_parameter inf_p
        } in
        (([p], Some(info.i32)), Some(info.strlen)) )
    | "strcmp" -> (
        let inf_p = {
          parameter_type = TY_array (TY_char);
          parameter_offset = 0;
          parameter_mode = PASS_BY_VALUE;
          parameter_value = Llvm.const_pointer_null (Llvm.pointer_type (Llvm.pointer_type info.i8));
        } in
        let p = {
          entry_id    = id_make "strcmp_var";
          entry_scope = sco;
          entry_info  = ENTRY_parameter inf_p
        } in
        (([p; p], Some(info.i32)), Some(info.strcmp)) )
    | "strcpy" -> (
        let inf_p = {
          parameter_type = TY_array (TY_char);
          parameter_offset = 0;
          parameter_mode = PASS_BY_VALUE;
          parameter_value = Llvm.const_pointer_null (Llvm.pointer_type (Llvm.pointer_type info.i8));
        } in
        let p = {
          entry_id    = id_make "strcpy_var";
          entry_scope = sco;
          entry_info  = ENTRY_parameter inf_p
        } in
        (([p; p], Some(info.void)), Some(info.strcpy)) )
    | "strcat" -> (
        let inf_p = {
          parameter_type = TY_array (TY_char);
          parameter_offset = 0;
          parameter_mode = PASS_BY_VALUE;
          parameter_value = Llvm.const_pointer_null (Llvm.pointer_type (Llvm.pointer_type info.i8));
        } in
        let p = {
          entry_id    = id_make "strcat_var";
          entry_scope = sco;
          entry_info  = ENTRY_parameter inf_p
        } in
        (([p; p], Some(info.void)), Some(info.strcat)) )
    | _ -> raise UnknownError
  ) in
  let f = {
    function_isForward = false;
    function_paramlist = par;
    function_redeflist = [];
    function_result = res;
    function_pstatus = PARDEF_DEFINE;
    function_initquad = 0;
    function_llvalue = var
  } in
  ignore (newEntry id (ENTRY_function f) false)



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
                                                        (*ignore (Llvm.build_ret_void info.builder);*)
                                                        let cur_bb2 = Llvm.insertion_block info.builder in
                                                        (if (Llvm.block_terminator cur_bb2) = None then
                                                           ( if ret_t = info.void then
                                                               ignore (Llvm.build_ret_void info.builder)
                                                             else ignore(Llvm.build_unreachable info.builder)) );
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
  | E_atom a                    -> (let p = compile_atom info a in
                                   match a with | A_call _ -> p
                                                | _        -> Llvm.build_load p "loadtmp" info.builder)
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
                                    else (
                                      Llvm.build_array_malloc (compile_type info a) t "arraytmp" info.builder ) )
  | E_nil                       -> Llvm.const_pointer_null info.tony_list
  | E_is_nil (e, line)          -> (let t = compile_expr info e in
                                    if (Llvm.classify_type (Llvm.element_type (Llvm.type_of t))) = Llvm.TypeKind.Struct then
                                      Llvm.build_is_null t "nulltmp" info.builder
                                    else (
                                      if Llvm.type_of t = info.tony_list then (*in this case we know it is null*)
                                        (let n = Llvm.const_null info.tony_list in
                                          Llvm.build_is_null n "nulltmp" info.builder
                                        )
                                      else (error "operator 'nil?' expected operand of type list";
                                            raise (TypeError line))) )
  | E_cons (e1, e2, line)       -> (let v1 = compile_expr info e1
                                    and v2 = compile_expr info e2 in
                                    if (Llvm.type_of v2) = info.tony_list || (Llvm.element_type (Llvm.type_of v2)) = info.tony_list then (
                                        let struct_t = (Llvm.struct_type info.context (Array.of_list([(Llvm.type_of v1); (Llvm.pointer_type info.tony_list)]))) in
                                        let l_node = Llvm.build_malloc struct_t "list_node" info.builder in
                                        let elem1 = Llvm.build_gep l_node [| (info.c32 0); (info.c32 0) |] "elem_1" info.builder in
                                        let elem2 = Llvm.build_gep l_node [| (info.c32 0); (info.c32 1) |] "elem_2" info.builder in
                                        let ptr_change = Llvm.build_bitcast v2 (Llvm.pointer_type info.tony_list) "tmpbitcast" info.builder in
                                        ignore (Llvm.build_store v1 elem1 info.builder);
                                        ignore (Llvm.build_store ptr_change elem2 info.builder);
                                        l_node
                                    )
                                    else (
                                      if (Llvm.classify_type (Llvm.element_type (Llvm.type_of v2))) = Llvm.TypeKind.Struct then
                                         let struct_t = Llvm.element_type (Llvm.type_of v2) in
                                         let el_t = Array.get (Llvm.struct_element_types struct_t) 0 in
                                         (*I want to compare types of v1 and el_t*)
                                         let (eq, btc) = equal_types info (Llvm.type_of v1) (el_t) in
                                         (*let struct_t = (if equal_types info (Llvm.struct_type info.context [|(Llvm.type_of v1); (Llvm.pointer_type info.tony_list)|]) (struct_t) then (
                                                           Llvm.struct_type info.context [|(Llvm.type_of v1); (Llvm.pointer_type info.tony_list)|])
                                                         else ( struct_t) ) in
                                         let v1_type = ( if equal_types info (el_t) (Llvm.type_of v1) then (
                                                           Llvm.struct_type info.context [| el_t; (Llvm.pointer_type info.tony_list) |])
                                                         else (Llvm.type_of v1) ) in*)
                                         if not(eq)
                                             then (error "type of list of right operand of operator '#' \
                                                          must be of same type as left operand";
                                                   (*(Printf.eprintf "%s, %s\n" (Llvm.string_of_lltype (el_t)) (Llvm.string_of_lltype (v1_type)));*)
                                                                                raise (TypeError line))
                                             else (
                                               let struct_t = (if btc = 2 then (
                                                                 Llvm.struct_type info.context [|(Llvm.type_of v1); (Llvm.pointer_type info.tony_list)|])
                                                              else ( struct_t) ) in
                                               let el_t = (if btc = 2 then
                                                              Llvm.type_of v1
                                                           else ( el_t) ) in
                                              let v1_type = ( if btc = 1 then
                                                                el_t
                                                              else (Llvm.type_of v1) ) in
                                               let l_node = Llvm.build_malloc struct_t "list_node" info.builder in
                                               let elem1 = Llvm.build_gep l_node [| (info.c32 0); (info.c32 0) |] "elem_1" info.builder in
                                               let elem2 = Llvm.build_gep l_node [| (info.c32 0); (info.c32 1) |] "elem_2" info.builder in
                                               let el_change = Llvm.build_bitcast v1 el_t "tmpbitcast1" info.builder in
                                               let ptr_change = Llvm.build_bitcast v2 (Llvm.pointer_type info.tony_list) "tmpbitcast2" info.builder in
                                               ignore (Llvm.build_store el_change elem1 info.builder);
                                               ignore (Llvm.build_store ptr_change elem2 info.builder);
                                               l_node
                                             )
                                      else (error "operator '#' expected a valid type and a list as operands";
                                            raise (TypeError line)) ) )
  | E_head (e, line)            -> (let v = compile_expr info e in
                                    if not(Llvm.is_null(v)) && (Llvm.classify_type (Llvm.element_type (Llvm.type_of v))) = Llvm.TypeKind.Struct then
                                      let ptr = Llvm.build_gep v [| (info.c64 0); (info.c32 0) |] "head_ptr" info.builder in
                                      Llvm.build_load ptr "head" info.builder
                                    else (error "operator 'head' expected operand of non-empty list type";
                                         raise (TypeError line)) )
  | E_tail (e, line)            -> (let v = compile_expr info e in
                                    if not(Llvm.is_null(v)) && (Llvm.classify_type (Llvm.element_type (Llvm.type_of v))) = Llvm.TypeKind.Struct then
                                     let ptr = Llvm.build_gep v [| (info.c64 0); (info.c32 1) |] "tail_ptr" info.builder in
                                     let tl = Llvm.build_load ptr "tail" info.builder in
                                     Llvm.build_bitcast tl (Llvm.type_of v) "tmpbitcast" info.builder
                                    else (error "operator 'tail' expected operand of non-empty list type";
                                          raise (TypeError line)) )


and check_param fname line info exp par =
    match par.entry_info with
      | ENTRY_parameter(pi) ->
        let a = (
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
        let arg = (
          if (Llvm.type_of a) = info.tony_list then (
            if (Llvm.classify_type (Llvm.element_type (Llvm.element_type (Llvm.type_of pi.parameter_value)))) = Llvm.TypeKind.Struct then
                Llvm.build_bitcast a (Llvm.element_type (Llvm.type_of pi.parameter_value)) "tmpbitcast" info.builder
            else (error "Cannot save a list in a non-list function parameter";
                  raise (TypeError line)) )
          else a
        ) in
        if pi.parameter_mode = PASS_BY_VALUE && (Llvm.type_of arg) <> (Llvm.element_type (Llvm.type_of pi.parameter_value)) then
         (error "parameter type in call of function '%s' inconsistent with type in function definition" fname;
          (*(Printf.eprintf "%s, %s\n" (Llvm.string_of_lltype (Llvm.type_of arg)) (Llvm.string_of_lltype (Llvm.type_of pi.parameter_value)));*)
          raise (TypeError line))
        else (if pi.parameter_mode <> PASS_BY_VALUE && (Llvm.type_of arg) <> (Llvm.type_of pi.parameter_value) then
                (error "parameter type in call of function '%s' inconsistent with type in function definition" fname;
                 (*(Printf.eprintf "%s, %s\n" (Llvm.string_of_lltype (Llvm.type_of arg)) (Llvm.string_of_lltype (Llvm.type_of pi.parameter_value)));*)
                 raise (TypeError line))
              else arg )
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
       let ret_void = (f.function_result = Some (info.void)) in
         begin
           try
             let params_array = Array.of_list (List.map2 (check_param nm line info) exprs f.function_paramlist) in (*???*)
             (if ret_void then Llvm.build_call (match f.function_llvalue with Some (v) -> v) params_array "" info.builder
              else
                 Llvm.build_call (match f.function_llvalue with Some (v) -> v) params_array "calltmp" info.builder)
           with Invalid_argument _ ->
             error "fewer or more parameters than expected in call of function '%s'" nm;
             raise (TypeError line)
         end
     else (error "function '%s' is not yet defined" nm;
           raise (TypeError line))
    | _ -> error "identifier '%s' is not a function" nm;
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
                           (*Printf.eprintf "%s %s\n" (Llvm.string_of_lltype (Llvm.type_of v)) (Llvm.string_of_lltype (Llvm.type_of n)));*)
                           begin
                             if (Llvm.classify_type (Llvm.element_type (Llvm.type_of v))) = Llvm.TypeKind.Pointer then
                               if (Llvm.type_of n) <> info.i32 then (error "array index not int";
                                                                   raise (TypeError line) )
                               else (
                                (*we need to keep info about array limits and have a sem fault if the program tries to break it*)
                                 let ar = Llvm.build_load v "loadtmp" info.builder in
                                 let ln = Llvm.array_length (Llvm.type_of ar) in
                                 (*Printf.eprintf "%d\n" (ln);
                                   (if (Llvm.array_length (Llvm.type_of ar) < 65) then*)
                                    Llvm.build_in_bounds_gep ar [| n |] "arraytmp" info.builder
                                  (*else
                                    Llvm.build_in_bounds_gep ar [| (info.c32 0) |] "arraytmp" info.builder)*))
                             else (error "identifier is not an array";
                                   raise (TypeError line))
                           end
  | A_call c            -> compile_call info c

and compile_simple info ast =
  match ast with
  | S_skip ()             -> () (*ignored by compiler? else must find suitable llvm instruction*)
  | S_assign (a, e, line) -> let x = compile_atom info a
                             and y = compile_expr info e in
                             let (eq, btc) = equal_types info (Llvm.element_type (Llvm.type_of x)) (Llvm.type_of y) in
                             if not(eq) then (error "lvalue and rvalue in assignment are not of the same type";
                                              (*(Printf.eprintf "%s, %s\n" (Llvm.string_of_lltype (Llvm.element_type (Llvm.type_of x))) (Llvm.string_of_lltype (Llvm.type_of new_y)));*)
                                              raise (TypeError line))
                             else (
                               let new_y = (if btc = 2 then Llvm.build_bitcast y (Llvm.element_type (Llvm.type_of x)) "tmpbitcast" info.builder
                                            else y) in
                               ignore (Llvm.build_store new_y x info.builder) (*if it is an array assingment i have to assign x the pointer*)
                             )

  | S_call c              -> let v = compile_call info c	in
                             if (Llvm.type_of v) <> info.void then raise UnknownError

and compile_stmt info ast =
    match ast with
    | S_simple s                                 -> compile_simple info s
    | S_exit line                                -> let bb = Llvm.insertion_block info.builder in
                                                    let f = Llvm.block_parent bb in
                                                    let f_ty = Llvm.type_of f in
                                                    let ret_ty = Llvm.return_type (Llvm.element_type f_ty) in
                                                    if ret_ty <> info.void then (error "type of object to be returned incompatible with void type while this is a void function";
                                                                                 (*(Printf.eprintf "%s\n" (Llvm.string_of_lltype ret_ty));*)
                                                                                 raise (TypeError line))
                                                    else ignore (Llvm.build_ret_void info.builder)
    | S_return (e, line)                         -> let t = compile_expr info e in
                                                    let bb = Llvm.insertion_block info.builder in
                                                    let f = Llvm.block_parent bb in
                                                    let f_ty = Llvm.type_of f in
                                                    let ret_ty = Llvm.return_type (Llvm.element_type f_ty) in
                                                    if (Llvm.type_of t) <> ret_ty then (error "type of object to be returned incompatible with return type of function";
                                                                                        (*(Printf.eprintf "%s %s\n" (Llvm.string_of_lltype (Llvm.type_of t)) (Llvm.string_of_lltype ret_ty));*)
                                                                                        raise (TypeError line))
                                                    else ignore (Llvm.build_ret t info.builder);
                                                    let new_bb =  Llvm.insert_block info.context "after_return" bb in
                                                    Llvm.move_block_after bb new_bb;
                                                    Llvm.position_at_end new_bb info.builder
    | S_if (e, stmts, elsif, els, line)          -> let v = compile_expr info e in
                                                    if (Llvm.type_of v) <> info.i1 then (error "condition in if-statement must be evaluated as boolean";
                                                                                         raise (TypeError line) )
                                                    else (
                                                      let cond = Llvm.build_icmp Llvm.Icmp.Ne v (info.c1 0) "if_cond" info.builder in
                                                      let bb = Llvm.insertion_block info.builder in
                                                      let f = Llvm.block_parent bb in
                                                      let then_bb = Llvm.append_block info.context "then" f in
                                                      let else_bb = Llvm.append_block info.context "else" f in
                                                      let after_bb = Llvm.append_block info.context "after" f in
                                                      ignore (Llvm.build_cond_br cond then_bb else_bb info.builder);
                                                      Llvm.position_at_end then_bb info.builder;
                                                      List.iter (compile_stmt info) stmts;
                                                      ignore (Llvm.build_br after_bb info.builder);
                                                      Llvm.position_at_end else_bb info.builder;
                                                      compile_elsif_stmt info else_bb after_bb elsif els;
                                                      Llvm.position_at_end after_bb info.builder );
    | S_for (simples1, e, simples2, stmts, line) -> List.iter (compile_simple info) simples1;
                                                    let bb = Llvm.insertion_block info.builder in
                                                    let f = Llvm.block_parent bb in
                                                    let loop_bb = Llvm.append_block info.context "loop" f in
                                                    let body_bb = Llvm.append_block info.context "body" f in
                                                    let after_bb = Llvm.append_block info.context "after" f in
                                                    ignore (Llvm.build_br loop_bb info.builder);
                                                    Llvm.position_at_end loop_bb info.builder;
                                                    let n = compile_expr info e in
                                                    if (Llvm.type_of n) <> info.i1 then (error "condition in for-statement must be evaluated as boolean";
                                                                                         raise (TypeError line) )
                                                    else (
                                                    (*let phi_iter = Llvm.build_phi [(n, bb)] "iter" info.builder in *)
                                                      let loop_cond = Llvm.build_icmp Llvm.Icmp.Ne n (info.c1 0) "loop_cond" info.builder in
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
                                                 let cond = Llvm.build_icmp Llvm.Icmp.Ne v (info.c1 0) "if_cond" info.builder in
                                                 let bb = Llvm.insertion_block info.builder in
                                                 let f = Llvm.block_parent bb in
                                                 let new_then_bb = Llvm.insert_block info.context "new_then" after_bb in
                                                 Llvm.move_block_after then_bb new_then_bb;
                                                 let else_bb = Llvm.insert_block info.context "else" after_bb in
                                                 Llvm.move_block_after new_then_bb else_bb;
                                                 ignore (Llvm.build_cond_br cond new_then_bb else_bb info.builder);
                                                 Llvm.position_at_end new_then_bb info.builder;
                                                 List.iter (compile_stmt info) stmts;
                                                 ignore (Llvm.build_br after_bb info.builder);
                                                 Llvm.position_at_end else_bb info.builder;
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
    Llvm.struct_set_body tony_list (Array.of_list ([Llvm.pointer_type i8; i64; (Llvm.pointer_type tony_list)])) true;
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
      Llvm.set_alignment 1 the_nl;*)
    (* Initialize library functions *)
    let puti_type =
      Llvm.function_type (Llvm.void_type context) [| i32 |] in
    let puti =
      Llvm.declare_function "puti" puti_type the_module in
    let putc_type =
      Llvm.function_type (Llvm.void_type context) [| i8 |] in
    let putc =
      Llvm.declare_function "putc" putc_type the_module in
    let putb_type =
      Llvm.function_type (Llvm.void_type context) [| i1 |] in
    let putb =
      Llvm.declare_function "putb" putb_type the_module in
    let puts_type =
      Llvm.function_type (Llvm.void_type context) [| Llvm.pointer_type i8 |] in
    let puts =
      Llvm.declare_function "puts" puts_type the_module in
    let geti_type =
      Llvm.function_type (i32) [| |] in
    let geti =
      Llvm.declare_function "geti" geti_type the_module in
    let getc_type =
      Llvm.function_type (i8) [| |] in
    let getc =
      Llvm.declare_function "getc" getc_type the_module in
    let getb_type =
      Llvm.function_type (i1) [| |] in
    let getb =
      Llvm.declare_function "getb" getb_type the_module in
    let gets_type =
      Llvm.function_type (Llvm.pointer_type i8) [| |] in
    let gets =
      Llvm.declare_function "gets" gets_type the_module in
    let abs_type =
      Llvm.function_type (i32) [| i32 |] in
    let abs =
      Llvm.declare_function "abs" abs_type the_module in
    let ord_type =
      Llvm.function_type (i32) [| i8 |] in
    let ord =
      Llvm.declare_function "ord" ord_type the_module in
    let chr_type =
      Llvm.function_type (i8) [| i32 |] in
    let chr =
      Llvm.declare_function "chr" chr_type the_module in
    let strlen_type =
      Llvm.function_type (i32) [| Llvm.pointer_type i8 |] in
    let strlen =
      Llvm.declare_function "strlen" strlen_type the_module in
    let strcmp_type =
      Llvm.function_type (i32) [| (Llvm.pointer_type i8); (Llvm.pointer_type i8) |] in
    let strcmp =
      Llvm.declare_function "strcmp" strcmp_type the_module in
    let strcpy_type =
      Llvm.function_type (Llvm.void_type context) [| (Llvm.pointer_type i8); (Llvm.pointer_type i8) |] in
    let strcpy =
      Llvm.declare_function "strcpy" strcpy_type the_module in
    let strcat_type =
      Llvm.function_type (Llvm.void_type context) [| (Llvm.pointer_type i8); (Llvm.pointer_type i8) |] in
    let strcat =
      Llvm.declare_function "strcat" strcat_type the_module in
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
      puti             = puti;
      putb             = putb;
      puts             = puts;
      putc             = putc;
      geti             = geti;
      getb             = getb;
      gets             = gets;
      getc             = getc;
      abs              = abs;
      ord              = ord;
      chr              = chr;
      strlen           = strlen;
      strcmp           = strcmp;
      strcpy           = strcpy;
      strcat           = strcat;
    } in
    let function_list = ["puti"; "putb"; "puts"; "putc"; "geti"; "getb"; "gets"; "getc"; "abs"; "ord"; "chr"; "strlen"; "strcmp"; "strcpy"; "strcat"] in
    List.iter (insert_built_in_function info) function_list;
    openScope i32;
    List.iter (compile_def info) defs;
    List.iter (compile_stmt info) stmts;
    ignore (Llvm.build_ret (c32 0) builder);
    closeScope ();
    (* Verify *)
    Llvm_analysis.assert_valid_module the_module;
    (* Optimize*)
    (*ignore (Llvm.PassManager.run_module the_module pm);*)
    (* Print out the IR *)
    Llvm.print_module "a.ll" the_module
    end
  | _ -> error "main function must be void and without arguments"

let compile ast = compile_func ast

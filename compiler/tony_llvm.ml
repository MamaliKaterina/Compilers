open Identifier
open Error
open Helping_types
open Tony_symbol

(* Helping function that returns whether the types of two parameters t1, t2
   are valid for a logical operation: they must be of the same type which
   might be int, bool, char.
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
    | i1  -> false
    | i8  -> false
    | i32 -> false
    | _   -> true )
  else true

(*Returns true if the two types can be considered equivalent*)
let rec equal_types info line x y =
  let x  = (if Llvm.classify_type x = Llvm.TypeKind.Pointer then Llvm.element_type x
            else x) in
  let y  = (if Llvm.classify_type y = Llvm.TypeKind.Pointer then Llvm.element_type y
            else y) in
  if x = y then 0 (*if they are not exactly the same type they still can be equivalent*)
  else if Llvm.classify_type x = Llvm.TypeKind.Struct && y = info.tony_list then 2
  else if Llvm.classify_type y = Llvm.TypeKind.Struct && x = info.tony_list then 1
  else if Llvm.classify_type x = Llvm.TypeKind.Struct && Llvm.classify_type y = Llvm.TypeKind.Struct then
    let el1 = Array.get (Llvm.struct_element_types x) 0 in
    let el2 = Array.get (Llvm.struct_element_types y) 0 in
    equal_types info line el1 el2
  else (
    error "Not equivalent types";
    raise (TypeError line)
  )

(*Needs changing, what about my child-functions*)
let malloc_outer_vars_struct current_struct info f depth =
  let d = (!currentScope.sco_nesting - depth) in
  if d = 0 then
      (*first param of function is its struct*)
      let f_struct_type = Llvm.type_of (Llvm.param (match f.function_llvalue with | Some v -> v) 0) in
      let f_struct = Llvm.build_malloc (Llvm.element_type f_struct_type) "vars_struct" info.builder in
      let off =
        match current_struct with
        | None -> 0
        | Some s ->
            let previous_struct_ptr = Llvm.build_struct_gep f_struct 0 "prev_struct_ptr" info.builder in
            ignore (Llvm.build_store s previous_struct_ptr info.builder);
            1
      in
      let store_var v =
        let var_value = (match v.variable_value with Some v -> v) in
        let dest_ptr = Llvm.build_struct_gep f_struct (v.variable_offset+off) "dest_elem_ptr" info.builder in
        ignore(Llvm.build_store var_value dest_ptr info.builder)
      in
      List.iter store_var (get_current_vars_list ());
      f_struct
  else
      let rec get_struct_at_depth strct depth =
        match depth with
        | 1 -> strct
        | d ->
          let new_strct = Llvm.build_struct_gep strct 0 "elem_ptr" info.builder in
          get_struct_at_depth (Llvm.build_load new_strct "next_struct" info.builder) (d-1)
      in (
      match current_struct with
      | Some s -> get_struct_at_depth s d )



let get_current_struct line =
  let current_fun_name = (match !currentScope.fun_name with | Some name -> name ) in
  let id = id_make current_fun_name in
  let e = lookupEntry id LOOKUP_ALL_SCOPES true in
  match e.entry_info with
  | ENTRY_function(f) -> Llvm.param (match f.function_llvalue with | Some v -> v) 0
  | _ -> (
      error "Non-function name given";
      raise (TypeError line)
    )


let get_struct_var line info e var_offset =
  let rec get_struct_at_depth strct depth =
    (match depth with
     | 1 -> strct
     | d -> (
         let new_strct = Llvm.build_struct_gep strct 0 "elem_ptr" info.builder in
         get_struct_at_depth (Llvm.build_load new_strct "next_struct" info.builder) (d-1))
    ) in
  match !currentScope.fun_name with
  | Some fu -> (
      let depth = !currentScope.sco_nesting - e.entry_scope.sco_nesting in
      let strct = get_struct_at_depth (get_current_struct line) depth in
      let off = if e.entry_scope.sco_nesting = 0 then 0 else 1 in
      let elem_ptr = Llvm.build_struct_gep strct (var_offset+off) "elem_ptr" info.builder in
      let val_ptr = Llvm.build_load elem_ptr "elem" info.builder in
      val_ptr
    )


let insert_built_in_function info nm =
  let id =  id_make nm in
  (*create fake scope for parameters*)
  let sco = {
    sco_parent = None;
    sco_nesting = -1;
    sco_entries = [];
    sco_negofs = 8;
    return_value = Null;
    fun_name = None
  } in
  let ((par, res), var) = (
    match nm with
    | "puti" -> (
        let inf_p = {
          parameter_type = TY_int;
          parameter_offset = 0;
          parameter_mode = PASS_BY_VALUE;
          parameter_value = Some (Llvm.const_pointer_null (Llvm.pointer_type info.i32));
        } in
        let p = {
          entry_id    = id_make "puti_var";
          entry_scope = sco;
          entry_info  = ENTRY_parameter inf_p
        } in
        (([p], Some(Null)), Some(info.puti)) )
    | "putc" -> (
        let inf_p = {
          parameter_type = TY_char;
          parameter_offset = 0;
          parameter_mode = PASS_BY_VALUE;
          parameter_value = Some (Llvm.const_pointer_null (Llvm.pointer_type info.i8));
        } in
        let p = {
          entry_id    = id_make "putc_var";
          entry_scope = sco;
          entry_info  = ENTRY_parameter inf_p
        } in
        (([p], Some(Null)), Some(info.putc)) )
    | "putb" -> (
        let inf_p = {
        parameter_type = TY_bool;
        parameter_offset = 0;
        parameter_mode = PASS_BY_VALUE;
        parameter_value = Some (Llvm.const_pointer_null (Llvm.pointer_type info.i1));
        } in
        let p = {
          entry_id    = id_make "putb_var";
          entry_scope = sco;
          entry_info  = ENTRY_parameter inf_p
        } in
        (([p], Some(Null)), Some(info.putb)) )
    | "puts" -> (
        let inf_p = {
          parameter_type = TY_array (TY_char);
          parameter_offset = 0;
          parameter_mode = PASS_BY_VALUE;
          parameter_value = Some (Llvm.const_pointer_null (Llvm.pointer_type (Llvm.pointer_type info.i8)));
        } in
        let p = {
          entry_id    = id_make "puts_var";
          entry_scope = sco;
          entry_info  = ENTRY_parameter inf_p
        } in
        (([p], Some(Null)), Some(info.puts)) )
    | "geti" -> (([], Some(TY_int)), Some(info.geti))
    | "getc" -> (([], Some(TY_char)), Some(info.getc))
    | "getb" -> (([], Some(TY_bool)), Some(info.getb))
    | "gets" -> (
        let inf_p1 = {
          parameter_type = TY_int;
          parameter_offset = 0;
          parameter_mode = PASS_BY_VALUE;
          parameter_value = Some (Llvm.const_pointer_null (Llvm.pointer_type info.i32));
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
          parameter_value = Some (Llvm.const_pointer_null (Llvm.pointer_type (Llvm.pointer_type info.i8)));
        } in
        let p2 = {
          entry_id    = id_make "gets_var2";
          entry_scope = sco;
          entry_info  = ENTRY_parameter inf_p2
        } in
        (([p1; p2], Some(Null)), Some(info.gets))
      )
    | "abs" -> (
        let inf_p = {
          parameter_type = TY_int;
          parameter_offset = 0;
          parameter_mode = PASS_BY_VALUE;
          parameter_value = Some (Llvm.const_pointer_null (Llvm.pointer_type info.i32));
        } in
        let p = {
          entry_id    = id_make "abs_var";
          entry_scope = sco;
          entry_info  = ENTRY_parameter inf_p
        } in
        (([p], Some(TY_int)), Some(info.abs)) )
    | "ord" -> (
        let inf_p = {
          parameter_type = TY_char;
          parameter_offset = 0;
          parameter_mode = PASS_BY_VALUE;
          parameter_value = Some (Llvm.const_pointer_null (Llvm.pointer_type info.i8));
        } in
        let p = {
          entry_id    = id_make "ord_var";
          entry_scope = sco;
          entry_info  = ENTRY_parameter inf_p
        } in
        (([p], Some(TY_int)), Some(info.ord)) )
    | "chr" -> (
        let inf_p = {
          parameter_type = TY_int;
          parameter_offset = 0;
          parameter_mode = PASS_BY_VALUE;
          parameter_value = Some (Llvm.const_pointer_null (Llvm.pointer_type info.i32));
        } in
        let p = {
          entry_id    = id_make "chr_var";
          entry_scope = sco;
          entry_info  = ENTRY_parameter inf_p
        } in
        (([p], Some(TY_char)), Some(info.chr)) )
    | "strlen" -> (
        let inf_p = {
          parameter_type = TY_array (TY_char);
          parameter_offset = 0;
          parameter_mode = PASS_BY_VALUE;
          parameter_value = Some (Llvm.const_pointer_null (Llvm.pointer_type (Llvm.pointer_type info.i8)));
        } in
        let p = {
          entry_id    = id_make "strlen_var";
          entry_scope = sco;
          entry_info  = ENTRY_parameter inf_p
        } in
        (([p], Some(TY_int)), Some(info.strlen)) )
    | "strcmp" -> (
        let inf_p = {
          parameter_type = TY_array (TY_char);
          parameter_offset = 0;
          parameter_mode = PASS_BY_VALUE;
          parameter_value = Some (Llvm.const_pointer_null (Llvm.pointer_type (Llvm.pointer_type info.i8)));
        } in
        let p = {
          entry_id    = id_make "strcmp_var";
          entry_scope = sco;
          entry_info  = ENTRY_parameter inf_p
        } in
        (([p; p], Some(TY_int)), Some(info.strcmp)) )
    | "strcpy" -> (
        let inf_p = {
          parameter_type = TY_array (TY_char);
          parameter_offset = 0;
          parameter_mode = PASS_BY_VALUE;
          parameter_value = Some (Llvm.const_pointer_null (Llvm.pointer_type (Llvm.pointer_type info.i8)));
        } in
        let p = {
          entry_id    = id_make "strcpy_var";
          entry_scope = sco;
          entry_info  = ENTRY_parameter inf_p
        } in
        (([p; p], Some(Null)), Some(info.strcpy)) )
    | "strcat" -> (
        let inf_p = {
          parameter_type = TY_array (TY_char);
          parameter_offset = 0;
          parameter_mode = PASS_BY_VALUE;
          parameter_value = Some (Llvm.const_pointer_null (Llvm.pointer_type (Llvm.pointer_type info.i8)));
        } in
        let p = {
          entry_id    = id_make "strcat_var";
          entry_scope = sco;
          entry_info  = ENTRY_parameter inf_p
        } in
        (([p; p], Some(Null)), Some(info.strcat)) )
    | "GC_init" -> (([], Some(Null)), Some(info.gc_init))
    | "GC_malloc" -> (
        let inf_p = {
          parameter_type = TY_int;
          parameter_offset = 0;
          parameter_mode = PASS_BY_VALUE;
          parameter_value = Some (Llvm.const_pointer_null (Llvm.pointer_type info.i32));
        } in
        let p = {
          entry_id    = id_make "gc_malloc_var";
          entry_scope = sco;
          entry_info  = ENTRY_parameter inf_p
        } in
        (([p], Some(TY_array(TY_char))), Some(info.gc_malloc)) )
    | _ -> raise UnknownError
  ) in
  let f = {
    function_isForward = false;
    function_paramlist = par;
    function_redeflist = [];
    function_result = res;
    function_pstatus = PARDEF_DEFINE;
    function_initquad = 0;
    function_llvalue = var;
    outer_scope_list = get_current_vars_list ()
  } in
  ignore (newEntry id (ENTRY_function f) false)



let compile_formal info f fu offset index formal =
  match formal with
  | Formal(parPas, t, slist) ->
    let ty = typ_to_lltype info t in
    match parPas with
    | BY_val ->
        let callnewParam i id = (
          let new_id = id_make id in
          let param_llvalue = Llvm.param fu (index+offset+i) in
          Llvm.set_value_name id param_llvalue;
          let var_llvalue = Llvm.build_malloc ty "malloc_var" info.builder in
          ignore (Llvm.build_store param_llvalue var_llvalue info.builder);
          ignore (newParameter true f PASS_BY_VALUE t (Some var_llvalue) new_id)
        ) in List.iteri callnewParam slist
    | BY_ref ->
        let callnewParam i id = (
          let new_id = id_make id in
          let param_llvalue = Llvm.param fu (index+offset+i) in
          Llvm.set_value_name id param_llvalue;
          ignore (newParameter true f PASS_BY_REFERENCE t (Some param_llvalue) new_id)
        ) in List.iteri callnewParam slist

let sem_formal f formal =
  match formal with
  | Formal(parPas, t, slist) ->
    let idlist = List.map id_make slist in
    let par_pas = (
      match parPas with
      | BY_val -> PASS_BY_VALUE
      | BY_ref -> PASS_BY_REFERENCE
    ) in
    ignore( List.map (newParameter true f par_pas t None) idlist)

let rec params_type info res frm =
  match frm with
  | [] -> List.rev res
  | hd::tl -> (
      match hd with
      | Formal (parPas, t, nms) ->
        let ty = typ_to_lltype info t in
        let ty = (
          if parPas = BY_ref then Llvm.pointer_type ty
          else ty
        ) in (*what if an array is passed by ref?*)
        let new_res = List.rev_append (List.rev_map (fun _ -> ty) nms) res in
        params_type info new_res tl
    )

let declare_function info nm tOp formals =
  let ret_t = (
    match tOp with
    | Some t -> typ_to_lltype info t
    | None -> info.void
  ) in
  let functions_actual_params = params_type info [] formals in
  let par_ar = (
    match (get_current_vars info.context info) with
    | Some vars ->
      Array.of_list (vars::functions_actual_params)
    | None ->
      Array.of_list (functions_actual_params)
  ) in
  let ty = Llvm.function_type ret_t par_ar in
  let scope = string_of_int !currentScope.sco_nesting in
  Llvm.declare_function (nm^scope) ty info.the_module

let rec compile_func_def info ast =
  match ast with
  | Func_def (Header(tOp, nm, formals), defs, stmts) ->
    (*Creating new basic block*)
    let cur_bb = Llvm.insertion_block info.builder in
    let cur_f = Llvm.block_parent cur_bb in
    let next_bb = Llvm.append_block info.context "return" cur_f in
    ignore (Llvm.build_br next_bb info.builder);
    (*Define function*)
    let id = id_make nm in
    let f = newFunction id true in
    let fu =
      match f.entry_info with
      | ENTRY_function inf when inf.function_pstatus = PARDEF_DEFINE ->
        declare_function info nm tOp formals
      | ENTRY_function inf ->
        match inf.function_llvalue with | Some v -> v
    in
    (*Write function llvm code in the function basic block*)
    let bb = Llvm.append_block info.context "entry" fu in
    Llvm.position_at_end bb info.builder;
    (*Open new scope before defining any variables*)
    let f_typ = (match tOp with |Some(t) -> t |None -> Null) in
    openScope f_typ nm ;
    (*Compile outer_scope_vars*)
    let off = if !currentScope.sco_nesting = 0 then 0 else 1 in
    List.iteri (compile_formal info f fu off) formals;
    endFunctionHeader f f_typ (Some fu);
    (*Compile function body*)
    List.iter (compile_def info) defs;
    List.iter (compile_stmt info) stmts;
    (*If function returns void, do a return void at the end always.
      Esle make an unreachable command, as the function should return before.*)
    let function_return_type = Llvm.return_type (Llvm.element_type (Llvm.type_of fu)) in
    let cur_bb2 = Llvm.insertion_block info.builder in
    if (Llvm.block_terminator cur_bb2) = None then (
      if function_return_type = info.void then ignore (Llvm.build_ret_void info.builder)
      else ignore(Llvm.build_unreachable info.builder)
    );
    Llvm.position_at_end next_bb info.builder;
    (*closing variables' scope*)
    closeScope false

and compile_func_decl info ast =
  match ast with
  | Func_decl (Header(tOp, nm, formals)) ->
    (*Define the function*)
    let id = id_make nm in
    let f = newFunction id true in
    let fu = declare_function info nm tOp formals in
    (*open "fake" scope for the parameters' temporary definition*)
    let f_typ = (match tOp with |Some(t) -> t |None -> Null) in
    openScope f_typ nm;
    (*"informally" define parameters to use in calling the function*)
    List.iter (sem_formal f) formals;
    (*Function MUST be fully defined*)
    forwardFunction f;
    endFunctionHeader f f_typ (Some fu);
    (*closing scope*)
    closeScope false

and compile_var_def info ast =
  match ast with
  | Var_def(t, s_list) ->
    let ty = typ_to_lltype info t in
    let callnewParam id = (
      let new_id = id_make id in
      let v = Llvm.build_malloc ty "malloc_var" info.builder in
      ignore ( newVariable true t (Some v) new_id )
    ) in
    List.iter callnewParam s_list


and compile_def info ast =
  match ast with
  | F_def fdef   -> compile_func_def info fdef
  | F_decl fdecl -> compile_func_decl info fdecl
  | V_def vdef   -> compile_var_def info vdef

and compile_expr info ast =
  (*cannot avoid raising an exception; otherwise, compiler throws error expecting type typ*)
  match ast with
  | E_atom a ->
    let p = compile_atom info a in
    Llvm.build_load p "loadtmp" info.builder
  | E_int_const n           -> info.c32 n (*64-bit integers?*)
  | E_char_const c          -> info.c8 (Char.code c)
  | E_un_plus (e, line)     -> compile_expr info e
  | E_un_minus (e, line)    ->
    let t = compile_expr info e in
    Llvm.build_neg t "negtmp" info.builder
  | E_op (e1, op, e2, line) ->
    let t1 = compile_expr info e1
    and t2 = compile_expr info e2 in (
      match op with
      | O_plus  -> Llvm.build_add t1 t2 "addtmp" info.builder
      | O_minus -> Llvm.build_sub t1 t2 "subtmp" info.builder
      | O_times -> Llvm.build_mul t1 t2 "multmp" info.builder
      | O_div   -> Llvm.build_sdiv t1 t2 "divtmp" info.builder
      | O_mod   -> Llvm.build_srem t1 t2 "modtmp" info.builder
    )
  | E_lg_op (e1, op, e2, line) ->
    let t1 = compile_expr info e1
    and t2 = compile_expr info e2 in (
      match op with
      | LO_eq 			    -> Llvm.build_icmp Llvm.Icmp.Eq t1 t2 "eqtmp" info.builder
      | LO_dif			    -> Llvm.build_icmp Llvm.Icmp.Ne t1 t2 "diftmp" info.builder
      | LO_less			    -> Llvm.build_icmp Llvm.Icmp.Slt t1 t2 "eqtmp" info.builder
      | LO_greater 		  -> Llvm.build_icmp Llvm.Icmp.Sgt t1 t2 "eqtmp" info.builder
      | LO_less_eq 		  -> Llvm.build_icmp Llvm.Icmp.Sle t1 t2 "eqtmp" info.builder
      | LO_greater_eq 	-> Llvm.build_icmp Llvm.Icmp.Sge t1 t2 "eqtmp" info.builder
    )
  | E_bool b        -> info.c1 (if b = True then 1 else 0)
  | E_not (e, line) ->
    let t = compile_expr info e in
    Llvm.build_not t "nottmp" info.builder
  | E_and_or (e1, ao, e2, line) ->
    let t1 = compile_expr info e1
    and t2 = compile_expr info e2 in (
      match ao with
      | And -> (
          (*Short-Circuit*)
          let cond = Llvm.build_icmp Llvm.Icmp.Ne t1 (info.c1 1) "if_cond" info.builder in
          let bb = Llvm.insertion_block info.builder in
          let f = Llvm.block_parent bb in
          let then_bb = Llvm.append_block info.context "then" f in
          let else_bb = Llvm.append_block info.context "else" f in
          let after_bb = Llvm.append_block info.context "after" f in
          ignore (Llvm.build_cond_br cond then_bb else_bb info.builder);
          Llvm.position_at_end then_bb info.builder;
          ignore (Llvm.build_br after_bb info.builder);
          Llvm.position_at_end else_bb info.builder;
          let and_var = Llvm.build_and t1 t2 "andtmp" info.builder in
          ignore (Llvm.build_br after_bb info.builder);
          Llvm.position_at_end after_bb info.builder;
          Llvm.build_phi [ (info.c1 0, then_bb); (and_var, else_bb) ] "phi_node" info.builder
        )
      | Or  -> (
        (*Short-Circuit*)
        let cond = Llvm.build_icmp Llvm.Icmp.Ne t1 (info.c1 0) "if_cond" info.builder in
        let bb = Llvm.insertion_block info.builder in
        let f = Llvm.block_parent bb in
        let then_bb = Llvm.append_block info.context "then" f in
        let else_bb = Llvm.append_block info.context "else" f in
        let after_bb = Llvm.append_block info.context "after" f in
        ignore (Llvm.build_cond_br cond then_bb else_bb info.builder);
        Llvm.position_at_end then_bb info.builder;
        ignore (Llvm.build_br after_bb info.builder);
        Llvm.position_at_end else_bb info.builder;
        let or_var = Llvm.build_or t1 t2 "ortmp" info.builder in
        ignore (Llvm.build_br after_bb info.builder);
        Llvm.position_at_end after_bb info.builder;
        Llvm.build_phi [ (info.c1 1, then_bb); (or_var, else_bb) ] "phi_node" info.builder
        )
    )
  | E_new (a, e, line) ->
    let t = compile_expr info e in
    let typofa = typ_to_lltype info a in
    let sizeofa = Llvm.size_of typofa in
    let siza32 = Llvm.build_bitcast sizeofa info.i32 "typesize" info.builder in
    let mallocsize = Llvm.build_mul t siza32 "mallocsize" info.builder in
    let gccall = Llvm.build_call info.gc_malloc [| mallocsize |] "gcmalloccall" info.builder in
    Llvm.build_bitcast gccall (Llvm.pointer_type typofa) "arraynew" info.builder
  | E_nil              -> Llvm.const_pointer_null info.tony_list
  | E_is_nil (e, line) -> (
      let t = compile_expr info e in
      if (Llvm.classify_type (Llvm.element_type (Llvm.type_of t))) = Llvm.TypeKind.Struct then
          Llvm.build_is_null t "nulltmp" info.builder
      else
          (*in this case we know it is null*)
          let n = Llvm.const_null info.tony_list in
          Llvm.build_is_null n "nulltmp" info.builder
    )
  | E_cons (e1, e2, line) ->
      (*Four cases:
        Null # Null
        sth # Null
        Null # sth_list
        sth # sth_list
      *)
      let v1 = compile_expr info e1
      and v2 = compile_expr info e2 in
      if (Llvm.type_of v2) = info.tony_list || (Llvm.element_type (Llvm.type_of v2)) = info.tony_list then (
        (*v2 is Null*)
        let ptr_v1 = (
          if v1 = Llvm.const_pointer_null info.tony_list then
            (*v1 is Null*)
            Llvm.build_bitcast v1 (Llvm.pointer_type (Llvm.type_of v1)) "tmpbitcast" info.builder
          else (*v1 is sth*) v1
        ) in
        let struct_t = (Llvm.struct_type info.context (Array.of_list([(Llvm.type_of ptr_v1); (Llvm.pointer_type info.tony_list)]))) in
        let gccall = Llvm.build_call (info.gc_malloc) [| info.c32 16 |] "gcmalloccall" info.builder in
        let l_node = Llvm.build_bitcast gccall (Llvm.pointer_type struct_t) "list_node" info.builder in
        let elem1 = Llvm.build_gep l_node [| (info.c32 0); (info.c32 0) |] "elem_1" info.builder in
        let elem2 = Llvm.build_gep l_node [| (info.c32 0); (info.c32 1) |] "elem_2" info.builder in
        let ptr_change = Llvm.build_bitcast v2 (Llvm.pointer_type info.tony_list) "tmpbitcast" info.builder in
        ignore (Llvm.build_store ptr_v1 elem1 info.builder);
        ignore (Llvm.build_store ptr_change elem2 info.builder);
        l_node
      )
      else (
        (*v2 is sth_list*)
        let struct_t = Llvm.element_type (Llvm.type_of v2) in
        let el_t = Array.get (Llvm.struct_element_types struct_t) 0 in
        (*I want to compare types of v1 and el_t*)
        let btc = equal_types info line (Llvm.type_of v1) (el_t) in
        let struct_t = if btc = 2 then
                          Llvm.struct_type info.context [|(Llvm.type_of v1); (Llvm.pointer_type info.tony_list)|]
                        else struct_t in
        let el_t = (if btc = 2 then Llvm.type_of v1 else el_t) in
        let v1_type = if btc = 1 then el_t else Llvm.type_of v1 in
        let gccall = Llvm.build_call (info.gc_malloc) [| info.c32 16 |] "gcmalloccall" info.builder in
        let l_node = Llvm.build_bitcast gccall (Llvm.pointer_type struct_t) "list_node" info.builder in
        let elem1 = Llvm.build_gep l_node [| (info.c32 0); (info.c32 0) |] "elem_1" info.builder in
        let elem2 = Llvm.build_gep l_node [| (info.c32 0); (info.c32 1) |] "elem_2" info.builder in
        let el_change = Llvm.build_bitcast v1 el_t "tmpbitcast1" info.builder in
        let ptr_change = Llvm.build_bitcast v2 (Llvm.pointer_type info.tony_list) "tmpbitcast2" info.builder in
        ignore (Llvm.build_store el_change elem1 info.builder);
        ignore (Llvm.build_store ptr_change elem2 info.builder);
        l_node
      )
  | E_head (e, line) ->
      let v = compile_expr info e in
      let ptr = Llvm.build_gep v [| (info.c64 0); (info.c32 0) |] "head_ptr" info.builder in
      Llvm.build_load ptr "head" info.builder
  | E_tail (e, line) ->
      let v = compile_expr info e in
      let ptr = Llvm.build_gep v [| (info.c64 0); (info.c32 1) |] "tail_ptr" info.builder in
      let tl = Llvm.build_load ptr "tail" info.builder in
      Llvm.build_bitcast tl (Llvm.type_of v) "tmpbitcast" info.builder


and check_param fname line info exp par =
    match par.entry_info with
      | ENTRY_parameter(pi) ->
        let a = (
          match pi.parameter_mode, exp with
          | ( PASS_BY_REFERENCE, E_atom (a) ) -> compile_atom info a
          | _ -> compile_expr info exp
        ) in
        if (Llvm.type_of a) = info.tony_list then
          let par_type = Llvm.type_of (match pi.parameter_value with | Some p -> p) in
          Llvm.build_bitcast a (Llvm.element_type par_type) "tmpbitcast" info.builder
        else a
     | _ ->
       raise UnknownError

and compile_call info c =
  match c with
  | C_call(nm, exprs, line) ->
    let id = id_make nm in
    let e = lookupEntry id LOOKUP_ALL_SCOPES true in
    let depth = e.entry_scope.sco_nesting in (
      match e.entry_info with
      | ENTRY_function(f) ->
      (*Getting function parameters*)
        let formal_params_array = List.map2 (check_param nm line info) exprs f.function_paramlist in (*???*)
        (*Add the outer variables to parameters struct if necessery*)
        let params_array = (
          match depth with
            (*If -1 it is an built-in function*)
          | -1 -> Array.of_list formal_params_array
          | _ -> (
              (*Main-level-0 does not have a struct*)
            if !currentScope.sco_nesting = 0 then
              let callee_struct = malloc_outer_vars_struct None info f depth in
              Array.of_list (callee_struct::formal_params_array)
            else
              let current_struct = get_current_struct line in
              let callee_struct = malloc_outer_vars_struct (Some current_struct) info f depth in
              Array.of_list (callee_struct::formal_params_array)
          )
        ) in
      (*Calling the function*)
        let ret_void = (f.function_result = Some (Null)) in
        let f_val = (match f.function_llvalue with Some (v) -> v) in (
        if ret_void then
          Llvm.build_call f_val params_array "" info.builder
        else
          let call = Llvm.build_call f_val params_array "calltmp" info.builder in
          (*Malloc memory for function's result*)
          let f_result = (match f.function_result with Some (v) -> (typ_to_lltype info v)) in
          let p = Llvm.build_malloc f_result "call_result" info.builder in
          ignore(Llvm.build_store call p info.builder);
          p
      )
      | _ -> (
          error "identifier '%s' is not a function" nm;
          raise (TypeError line)
        )
    )


and compile_atom info ast =
  match ast with
  | A_var (v, line) ->
    let id = id_make v in
    let e = lookupEntry id LOOKUP_ALL_SCOPES true in
    let (value, offset) = (
      match e.entry_info with
       | ENTRY_variable(v)  -> (v.variable_value, v.variable_offset)
       | ENTRY_parameter(v) -> (v.parameter_value, v.parameter_offset)
       | _                  -> (
           error "identifier '%s' is neither variable nor parameter" v;
           raise (TypeError line)
         )
    ) in
    if e.entry_scope.sco_nesting = !currentScope.sco_nesting then
      (match value with Some v -> v)
    else get_struct_var line info e offset
  | A_string_const str  ->
    let str = info.make_string str in (*when reading the string from input it does not contain the white character in the end?*)
    (info.global_counter) := !(info.global_counter) + 1;
    let num = string_of_int (!(info.global_counter)) in
    let name = (Printf.sprintf "string%s" num) in
    let ptr_name = (Printf.sprintf "string_ptr%s" num) in
    let glb = Llvm.define_global name str info.the_module in
    let first_elem_ptr = Llvm.build_gep glb [|(info.c32 0); (info.c32 0)|] "string" info.builder in
    Llvm.define_global ptr_name first_elem_ptr info.the_module
  | A_atom (a, e, line) ->
    let n = compile_expr info e
    and v = compile_atom info a in
    let ar = Llvm.build_load v "loadtmp" info.builder in
    Llvm.build_in_bounds_gep ar [| n |] "arraytmp" info.builder
  | A_call c -> compile_call info c


and compile_simple info ast =
  match ast with
  | S_skip () -> ()
  | S_assign (a, e, line) ->
    let x = compile_atom info a
    and y = compile_expr info e in
    let btc = equal_types info line (Llvm.element_type (Llvm.type_of x)) (Llvm.type_of y) in
    let new_y = (
      if btc = 2 then
        Llvm.build_bitcast y (Llvm.element_type (Llvm.type_of x)) "tmpbitcast" info.builder
      else y
    ) in
    ignore (Llvm.build_store new_y x info.builder)
  | S_call c ->
    let v = compile_call info c	in
    if (Llvm.type_of v) <> info.void then raise UnknownError


and compile_stmt info ast =
    match ast with
    | S_simple s -> compile_simple info s
    | S_exit line ->
      let bb = Llvm.insertion_block info.builder in
      let f = Llvm.block_parent bb in
      let f_ty = Llvm.type_of f in
      let ret_ty = Llvm.return_type (Llvm.element_type f_ty) in
      ignore (Llvm.build_ret_void info.builder);
      let new_bb =  Llvm.insert_block info.context "after_exit" bb in
      Llvm.move_block_after bb new_bb;
      Llvm.position_at_end new_bb info.builder
    | S_return (e, line) ->
      let t = compile_expr info e in
      let bb = Llvm.insertion_block info.builder in
      let f = Llvm.block_parent bb in
      let f_ty = Llvm.type_of f in
      let ret_ty = Llvm.return_type (Llvm.element_type f_ty) in
      let btc = equal_types info line (Llvm.type_of t) ret_ty in
      let new_t = if btc > 0 then Llvm.build_bitcast t ret_ty "tmpbitcast" info.builder else t in
      ignore (Llvm.build_ret new_t info.builder);
      let new_bb =  Llvm.insert_block info.context "after_return" bb in
      Llvm.move_block_after bb new_bb;
      Llvm.position_at_end new_bb info.builder
    | S_if (e, stmts, elsif, els, line) ->
      let v = compile_expr info e in
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
      Llvm.position_at_end after_bb info.builder
    | S_for (simples1, e, simples2, stmts, line) ->
      List.iter (compile_simple info) simples1;
      let bb = Llvm.insertion_block info.builder in
      let f = Llvm.block_parent bb in
      let loop_bb = Llvm.append_block info.context "loop" f in
      let body_bb = Llvm.append_block info.context "body" f in
      let after_bb = Llvm.append_block info.context "after" f in
      ignore (Llvm.build_br loop_bb info.builder);
      Llvm.position_at_end loop_bb info.builder;
      let n = compile_expr info e in
      let loop_cond = Llvm.build_icmp Llvm.Icmp.Ne n (info.c1 0) "loop_cond" info.builder in
      ignore (Llvm.build_cond_br loop_cond body_bb after_bb info.builder);
      Llvm.position_at_end body_bb info.builder;
      List.iter (compile_stmt info) stmts;
      List.iter (compile_simple info) simples2;
      ignore (Llvm.build_br loop_bb info.builder);
      Llvm.position_at_end after_bb info.builder


and compile_elsif_stmt info then_bb after_bb ast els =
  match ast with
  | Some( S_elsif (e, stmts, elsif, line) ) ->
    let v = compile_expr info e in
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
    compile_elsif_stmt info else_bb after_bb elsif els
  | None -> compile_else_stmt info then_bb after_bb els


and compile_else_stmt info then_bb after_bb ast =
  match ast with
  | Some ( S_else (stmts) ) ->
    Llvm.position_at_end then_bb info.builder;
    List.iter (compile_stmt info) stmts;
    ignore (Llvm.build_br after_bb info.builder)
  | None ->
    Llvm.position_at_end then_bb info.builder;
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
      Llvm.function_type (Llvm.void_type context) [| (i32); (Llvm.pointer_type i8) |] in
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
    let gc_init_type =
      Llvm.function_type (Llvm.void_type context) [| |] in
    let gc_init =
      Llvm.declare_function "GC_init" gc_init_type the_module in
    let gc_malloc_type =
      Llvm.function_type (Llvm.pointer_type i8) [| i32 |] in
    let gc_malloc =
      Llvm.declare_function "GC_malloc" gc_malloc_type the_module in
    (* Define and start and main function *)
    let main_type = Llvm.function_type void [| |] in
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
      gc_init          = gc_init;
      gc_malloc        = gc_malloc;
    } in
    initSymbolTable 0;
    let function_list = ["puti"; "putb"; "puts"; "putc"; "geti"; "getb"; "gets"; "getc"; "abs"; "ord"; "chr"; "strlen"; "strcmp"; "strcpy"; "strcat"; "GC_init"; "GC_malloc"] in
    List.iter (insert_built_in_function info) function_list;
    openScope TY_int "main";
    Llvm.build_call (info.gc_init) [| |] "" info.builder;
    List.iter (compile_def info) defs;
    List.iter (compile_stmt info) stmts;
    ignore (Llvm.build_ret_void builder);
    closeScope false;
    (* Verify *)
    Llvm_analysis.assert_valid_module the_module;
    (* Print out the IR *)
    Llvm.print_module "a.ll" the_module
    end
  | _ -> error "main function must be void and without arguments"

let compile ast = compile_func ast

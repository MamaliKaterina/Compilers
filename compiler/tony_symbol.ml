open Identifier
open Error
open Helping_types

module H = Hashtbl.Make (
  struct
    type t = id
    let equal = (==)
    let hash = Hashtbl.hash
  end
  )

type pass_mode = PASS_BY_VALUE | PASS_BY_REFERENCE

type param_status =
  | PARDEF_COMPLETE
  | PARDEF_DEFINE
  | PARDEF_CHECK

type scope = {
  sco_parent          : scope option;
  sco_nesting         : int;
  mutable sco_entries : entry list;
  mutable sco_negofs  : int;
  return_value        : Llvm.lltype option;
  outer_scope_vars    : Llvm.lltype option;
  current_function    : Llvm.llvalue option
}

and variable_info = {
  variable_type         : typ;
  variable_offset       : int;
  variable_value        : Llvm.llvalue; (*must be a ptr*)
  variable_scope        : int
}

and function_info = {
  mutable function_isForward : bool;
  mutable function_paramlist : entry list;
  mutable function_redeflist : entry list;
  mutable function_result    : Llvm.lltype option;
  mutable function_pstatus   : param_status;
  mutable function_initquad  : int;
  mutable function_llvalue   : Llvm.llvalue option;
  mutable outer_scope_list   : variable_info list;
  mutable outer_scope_vars   : Llvm.lltype option
}

and parameter_info = {
  mutable parameter_type    : typ;
  mutable parameter_offset  : int;
  parameter_mode            : pass_mode;
  parameter_value           : Llvm.llvalue
}

and temporary_info = {
  temporary_type   : typ;
  temporary_offset : int;
  temporary_value  : Llvm.llvalue
}

and entry_info = ENTRY_none
               | ENTRY_variable of variable_info
               | ENTRY_function of function_info
               | ENTRY_parameter of parameter_info
               | ENTRY_temporary of temporary_info

and entry = {
  entry_id    : Identifier.id;
  entry_scope : scope;
  entry_info  : entry_info
}

type lookup_type = LOOKUP_CURRENT_SCOPE | LOOKUP_ALL_SCOPES

let start_positive_offset = 8
let start_negative_offset = -1

let the_outer_scope = {
  sco_parent = None;
  sco_nesting = -1;
  sco_entries = [];
  sco_negofs = start_negative_offset;
  return_value = None;
  outer_scope_vars = None;
  current_function = None
}

let no_entry id = {
  entry_id = id;
  entry_scope = the_outer_scope;
  entry_info = ENTRY_none
}

let currentScope = ref the_outer_scope
let quadNext = ref 1
let tempNumber = ref 1

let tab = ref (H.create 0)

let initSymbolTable size =
  tab := H.create size;
  currentScope := the_outer_scope

let get_current_vars context =
  let entries = List.rev (!currentScope.sco_entries) in
  let get_entry e =
    (match e.entry_info with
     | ENTRY_variable v -> Some ((Llvm.type_of v.variable_value))
     | ENTRY_parameter p -> Some ((Llvm.type_of p.parameter_value)) (*??? ref vars*)
     | _ -> None
    ) in
  let outer_scope_params =
    (match !currentScope.outer_scope_vars with
     | Some v -> v::(List.filter_map get_entry entries)
     | None -> (List.filter_map get_entry entries) ) in
  match outer_scope_params with
  | [] -> None
  | _ -> Some (Llvm.pointer_type (Llvm.struct_type context (Array.of_list outer_scope_params)))

let get_current_vars_list () =
  let entries = List.rev (!currentScope.sco_entries) in
  let get_entry e =
    (match e.entry_info with
     | ENTRY_variable v -> Some v
     | ENTRY_parameter p ->
       (let v = {
         variable_type = p.parameter_type;
         variable_offset = p.parameter_offset;
         variable_value = p.parameter_value;
         variable_scope = !currentScope.sco_nesting
        } in
        Some v)
     | _ -> None
    ) in
  List.filter_map get_entry entries


(*for us a scope is a function... useful to keep return value*)
let openScope rv fu context =
  let sco = {
    sco_parent = Some !currentScope;
    sco_nesting = !currentScope.sco_nesting + 1;
    sco_entries = [];
    sco_negofs = start_negative_offset;
    return_value = Some rv;
    outer_scope_vars = get_current_vars context;
    current_function = fu
  } in
  (*)(Printf.eprintf "%d\n" (sco.sco_nesting) );*)
  currentScope := sco

let closeScope () =
  let sco = !currentScope in
  let manyentry e = H.remove !tab e.entry_id in
  List.iter manyentry sco.sco_entries;
  match sco.sco_parent with
  | Some scp ->
    currentScope := scp
  | None ->
    error "cannot close the outer scope!"

exception Failure_NewEntry of entry

let newEntry id inf err =
  try
    if err then begin
      try
        let e = H.find !tab id in
        if e.entry_scope.sco_nesting = !currentScope.sco_nesting then
          raise (Failure_NewEntry e)
      with Not_found ->
        ()
    end;
    let e = {
      entry_id = id;
      entry_scope = !currentScope;
      entry_info = inf
    } in
    H.add !tab id e;
    !currentScope.sco_entries <- e :: !currentScope.sco_entries;
    (*)(Printf.eprintf "%s %d\n" (id_name id) (e.entry_scope.sco_nesting) );*)
    e
  with Failure_NewEntry e ->
    error "duplicate identifier '%a'" pretty_id id;
    e

let lookupEntry id how err =
  let scc = !currentScope in
  let lookup () =
    match how with
    | LOOKUP_CURRENT_SCOPE ->
      let e = H.find !tab id in
      if e.entry_scope.sco_nesting = scc.sco_nesting then
        e
      else
        raise Not_found
    | LOOKUP_ALL_SCOPES ->
      H.find !tab id in
  if err then
    try
      lookup ()
    with Not_found ->
      error "unknown identifier '%a' (first occurrence)"
        pretty_id id;
      (* put it in, so we don't see more errors *)
      H.add !tab id (no_entry id);
      raise Exit
  else
    lookup ()

let newVariable err typ id v =
  !currentScope.sco_negofs <- !currentScope.sco_negofs + 1;
  let inf = {
    variable_type = typ;
    variable_offset = !currentScope.sco_negofs;
    variable_value = v;
    variable_scope = !currentScope.sco_nesting
  } in
  newEntry id (ENTRY_variable inf) err (*checking for double par happens inside newEntry func*)


let newFunction id context err =
  try
    let e = lookupEntry id LOOKUP_CURRENT_SCOPE false in (*we don't want to find it!*)
    match e.entry_info with
    | ENTRY_function inf when inf.function_isForward ->
      inf.function_isForward <- false;
      inf.function_pstatus <- PARDEF_CHECK;
      inf.function_redeflist <- inf.function_paramlist;
      e
    | _ ->
      if err then
        error "duplicate identifier '%a'" pretty_id id;
      raise Exit
  with Not_found ->
    let inf = {
      function_isForward = false;
      function_paramlist = [];
      function_redeflist = [];
      function_result = None;
      function_pstatus = PARDEF_DEFINE;
      function_initquad = 0;
      function_llvalue = None;
      outer_scope_list = get_current_vars_list ();
      outer_scope_vars = get_current_vars context
    } in
    newEntry id (ENTRY_function inf) false

let newParameter err f mode typ id v =
  match f.entry_info with
  | ENTRY_function inf -> begin
      match inf.function_pstatus with
      | PARDEF_DEFINE ->
        let inf_p = {
          parameter_type = typ;
          parameter_offset = 0;
          parameter_mode = mode;
          parameter_value = v
        } in
        let e = newEntry id (ENTRY_parameter inf_p) err in
        inf.function_paramlist <- e :: inf.function_paramlist;
        e
      | PARDEF_CHECK -> begin
          match inf.function_redeflist with
          | p :: ps -> begin
              inf.function_redeflist <- ps;
              match p.entry_info with
              | ENTRY_parameter inf ->
                if not (equalType (Llvm.type_of inf.parameter_value) (Llvm.element_type (Llvm.type_of v))) then
                  error "Parameter type mismatch in redeclaration \
                         of function %a" pretty_id f.entry_id
                (*Printf.eprintf "%s, %s\n" (Llvm.string_of_lltype (Llvm.type_of inf.parameter_value)) (Llvm.string_of_lltype (Llvm.type_of v))*)
                else if inf.parameter_mode != mode then
                  error "Parameter passing mode mismatch in redeclaration \
                         of function %a" pretty_id f.entry_id
                else if p.entry_id != id then
                  error "Parameter name mismatch in redeclaration \
                         of function %a" pretty_id f.entry_id
                else
                  H.add !tab id p; (*why add again? It must have been added at declare fun.*)
                p
              | _ ->
                error "I found a parameter that is not a parameter!";
                raise Exit
            end
          | [] ->
            error "More parameters than expected in redeclaration \
                   of function %a" pretty_id f.entry_id;
            raise Exit
        end
      | PARDEF_COMPLETE ->
        error "Cannot add a parameter to an already defined function";
        raise Exit
    end
  | _ ->
    error "Cannot add a parameter to a non-function";
    raise Exit

let newTemporary typ v =
  let id = id_make ("$" ^ string_of_int !tempNumber) in
  !currentScope.sco_negofs <- !currentScope.sco_negofs - sizeOfType typ;
  let inf = {
    temporary_type = typ;
    temporary_offset = !currentScope.sco_negofs;
    temporary_value = v
  } in
  incr tempNumber;
  newEntry id (ENTRY_temporary inf) false

let forwardFunction e =
  match e.entry_info with
  | ENTRY_function inf ->
    inf.function_isForward <- true
  | _ ->
    error "Cannot make a non-function forward"

let endFunctionHeader e fval typ =
  match e.entry_info with
  | ENTRY_function inf ->
    begin
      match inf.function_pstatus with
      | PARDEF_COMPLETE ->
        error "Cannot end parameters in an already defined function"
      | PARDEF_DEFINE ->
        inf.function_llvalue <- Some fval;
        inf.function_result <- Some typ;
        let offset = ref (start_negative_offset+1) in
        let fix_offset e =
          match e.entry_info with
          | ENTRY_parameter inf ->
            inf.parameter_offset <- !offset;
            (*let size =
              match inf.parameter_mode with
              | PASS_BY_VALUE     -> sizeOfType inf.parameter_type
              | PASS_BY_REFERENCE -> 2 in*)
            offset := !offset + 1
          | _ ->
            error "Cannot fix offset to a non parameter" in
        List.iter fix_offset (List.rev inf.function_paramlist);
        inf.function_paramlist <- List.rev inf.function_paramlist
      | PARDEF_CHECK ->
        inf.function_llvalue <- Some fval;
        if inf.function_redeflist <> [] then
          error "Fewer parameters than expected in redeclaration \
                 of function %a" pretty_id e.entry_id;
        let f_res = (match inf.function_result with
                     | Some (t) -> t
                     | None -> error "Function type doesn't exist";
                               raise Terminate) in
        if not (equalType f_res typ) then
          error "Result type mismatch in redeclaration of function %a"
            pretty_id e.entry_id;
    end;
    inf.function_pstatus <- PARDEF_COMPLETE
  | _ ->
    error "Cannot end parameters in a non-function"

let get_cur_return_value () =
  !currentScope.return_value

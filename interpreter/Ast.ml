type var = string
type oper = O_plus | O_minus | O_times | O_div | O_mod

type ast_stmt =
  | S_simple of ast_simple
  | S_return of ast_expr
  | S_for of ast_expr * ast_stmt
  | S_if of ast_expr * ast_stmt list * ast_elsif_stmt * ast_else_stmt
  | S_for of ast_simple list * ast_expr * ast_simple list * ast_stmt list

type ast_expr =
  | E_int_const of int
  | E_char_const of char
  |

let vars = Map.make((*something in here*))

let rec run_expr ast =
  match ast with
  | E_int_const n     -> n
  | E_char_const c    -> c
  (* below is just copy-pasted from Papaspirou
  | E_var x           -> vars.(int_of_char x - int_of_char 'a')
  | E_op (e1, op, e2) -> let v1 = run_expr e1
                         and v2 = run_expr e2 in
            		         match op with
            		         | O_plus  -> v1 + v2
            		         | O_minus -> v1 - v2
                         | O_times -> v1 * v2*)

let rec run_stmt ast =
  match ast with
  | S_simple s   -> let v = run_simple s in v
  | S_return e   -> let v = run_expr e in v
  (*below is just copy-pasted from Papaspirou
  | S_for (e, s) -> let v = run_expr e in
                    for i = 1 to v do
            		      run_stmt s
            		    done
  | S_block b    -> run b
  | S_if (e, s)  -> let v = run_expr e in
                    if v <> 0 then run_stmt s*)

and run asts = List.iter run_func_def asts

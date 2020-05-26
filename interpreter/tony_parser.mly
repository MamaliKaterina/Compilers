%{
	open Parsing
	open Helping_types
%}

%token T_eof
%token T_and
%token T_bool
%token T_char
%token T_decl
%token T_def
%token T_else
%token T_elsif
%token T_end
%token T_exit
%token T_false
%token T_for
%token T_head
%token T_if
%token T_int
%token T_list
%token T_mod
%token T_new
%token T_nil
%token T_is_nil
%token T_not
%token T_or
%token T_ref
%token T_return
%token T_skip
%token T_tail
%token T_true
%token<string> T_var
%token<int> T_int_const
%token<char> T_char_const
%token<string> T_string_const
%token T_eq
%token T_minus
%token T_plus
%token T_times
%token T_div
%token T_cons
%token T_dif
%token T_less
%token T_greater
%token T_less_eq
%token T_greater_eq
%token T_lbracket
%token T_rbracket
%token T_assign
%token T_lsqbracket
%token T_rsqbracket
%token T_colon
%token T_semicolon
%token T_comma


%left T_or
%left T_and
%nonassoc T_not
%nonassoc T_eq T_dif T_less T_greater T_less_eq T_greater_eq
%right T_cons
%left T_plus T_minus
%left T_times T_div T_mod
%nonassoc NT_plus NT_minus
/*
%left T_or
%left T_and
%nonassoc T_not
%nonassoc lgop
%right T_cons
%left op
%left T_plus T_minus
%left T_times T_div T_mod
%nonassoc NT_plus NT_minus*/


%start program
%type <Helping_types.ast_func_def> program
%type <ast_func_def> func_def
%type <ast_def list> def
%type <ast_stmt list> multi_stmt
%type <ast_header> header
%type <typ option> mytype
%type <ast_formal list> myformal
%type <ast_formal list> repformal
%type <ast_formal> formal
%type <paramPas> myref
%type <string list> other_id
%type <typ> ttype
%type <ast_func_decl> func_decl
%type <ast_var_def> var_def
%type <ast_stmt> stmt
%type <ast_elsif_stmt option> elsif_stmt
%type <ast_else_stmt option> else_stmt
%type <ast_simple> simple
%type <ast_simple list> simple_list
%type <ast_simple list> other_simple
%type <ast_call> call
%type <ast_expr list> other_expr
%type <ast_atom> atom
%type <ast_expr> expr
/*%type <operator> oper
%type <lg_operator> lg_oper*/

%%

program	: func_def T_eof	{ $1 }

func_def : T_def header T_colon def multi_stmt T_end 	{ Func_def ($2, $4, $5) }

def : /*nothing*/ 	{ [] }
	 | func_def def	{ (F_def $1)::$2 }
	 | func_decl def	{ (F_decl $1)::$2 }
	 | var_def def	{ (V_def $1)::$2 }

multi_stmt : stmt	{ [$1] }
		   | stmt multi_stmt	{ $1::$2 }

header : mytype T_var T_lbracket myformal T_rbracket	{ Header ($1, $2, $4) }

mytype : /*nothing*/ 	{ None }
	   | ttype	{ Some $1 }

myformal : /*nothing*/	{ [] }
		 | formal repformal	{ $1::$2 }

repformal : /*nothing*/	{ [] }
		  | T_semicolon formal repformal	{ $2::$3 }

formal : myref ttype T_var other_id	{ Formal ($1, $2, $3::$4) }

myref : /*nothing*/	{ BY_val }
	  | T_ref	{ BY_ref }

other_id : /*nothing*/	{ [] }
		 | T_comma T_var other_id	{ $2::$3 }

ttype : T_int	{ TY_int }
	 | T_bool	{ TY_bool }
	 | T_char	{ TY_char }
	 | ttype T_lsqbracket T_rsqbracket	{ TY_array $1 }
	 | T_list T_lsqbracket ttype T_rsqbracket	{ TY_list $3 }

func_decl : T_decl header	{ Func_decl $2 }

var_def : ttype T_var other_id	{ Var_def ($1, $2::$3) }

stmt : simple	{ S_simple $1 }
	 | T_exit	{ S_exit () }
	 | T_return expr	{ S_return $2 }
	 | T_if expr T_colon multi_stmt elsif_stmt else_stmt T_end	{ S_if ($2, $4, $5, $6) }
	 | T_for simple_list T_semicolon expr T_semicolon simple_list T_colon multi_stmt T_end	{ S_for ($2, $4, $6, $8) }

elsif_stmt : /*nothing*/	{ None }
		  | T_elsif expr T_colon multi_stmt elsif_stmt	{ Some (S_elsif ($2, $4, $5)) }

else_stmt : /*nothing*/	{ None }
		  | T_else T_colon multi_stmt	{ Some (S_else $3) }

simple : T_skip	{ S_skip () }
	   | atom T_assign expr	{ S_assign ($1, $3) }
	   | call	{ S_call $1 }

simple_list : simple other_simple	{ $1::$2 }

other_simple : /*nothing*/	{ [] }
			 | T_comma simple other_simple	{ $2::$3 }

call : T_var T_lbracket T_rbracket	{ C_call ($1, []) }
	 | T_var T_lbracket expr other_expr T_rbracket	{ C_call ($1, $3::$4) }

other_expr : /*nothing*/	{ [] }
		   | T_comma expr other_expr	{ $2::$3 }

atom : T_var	{ A_var $1 }
	 | T_string_const	{ A_string_const $1 }
	 | atom T_lsqbracket expr T_rsqbracket	{ A_atom ($1, $3) }
	 | call	{ A_call $1 }

expr : atom	{ E_atom $1 }
	 | T_int_const	{ E_int_const $1 }
	 | T_char_const	{ E_char_const $1 }
	 | T_lbracket expr T_rbracket	{ $2 }
	 | T_plus expr %prec NT_plus	{ E_un_plus $2 }
	 | T_minus expr %prec NT_minus	{ E_un_minus $2 }
	 | expr T_plus expr { E_op ($1, O_plus, $3, (Parsing.rhs_start_pos 2).pos_lnum) }
	 | expr T_minus expr { E_op ($1, O_minus, $3, (Parsing.rhs_start_pos 2).pos_lnum) }
	 | expr T_times expr { E_op ($1, O_times, $3, (Parsing.rhs_start_pos 2).pos_lnum) }
	 | expr T_div expr { E_op ($1, O_div, $3, (Parsing.rhs_start_pos 2).pos_lnum) }
	 | expr T_mod expr { E_op ($1, O_mod, $3, (Parsing.rhs_start_pos 2).pos_lnum) }
	 | expr T_eq expr { E_lg_op ($1, LO_eq, $3) }
	 | expr T_dif expr { E_lg_op ($1, LO_dif, $3) }
	 | expr T_less expr { E_lg_op ($1, LO_less, $3) }
	 | expr T_greater expr { E_lg_op ($1, LO_greater, $3) }
	 | expr T_less_eq expr { E_lg_op ($1, LO_less_eq, $3) }
	 | expr T_greater_eq expr { E_lg_op ($1, LO_greater_eq, $3) }
	 | T_true	{ E_bool True }
	 | T_false	{ E_bool False }
	 | T_not expr	{ E_not $2 }
	 | expr T_and expr	{ E_and_or ($1, And, $3) }
	 | expr T_or expr	{ E_and_or ($1, Or, $3) }
	 | T_new ttype T_lsqbracket expr T_rsqbracket	{ E_new ($2, $4) }
	 | T_nil	{ E_nil }
	 | T_is_nil T_lbracket expr T_rbracket	{ E_is_nil $3 }
	 | expr T_cons expr	{ E_cons ($1, $3) }
	 | T_head T_lbracket expr T_rbracket	{ E_head $3 }
	 | T_tail T_lbracket expr T_rbracket	{ E_tail $3 }

/*oper : T_plus { O_plus } | T_minus { O_minus } | T_times { O_times } | T_div { O_div } | T_mod { O_mod }

lg_oper : T_eq { LO_eq } | T_dif { LO_dif } | T_less { LO_less } | T_greater { LO_greater } | T_less_eq { LO_less_eq } | T_greater_eq { LO_greater_eq }*/

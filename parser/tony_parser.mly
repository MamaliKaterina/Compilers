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
%token T_var
%token T_int_const
%token T_char_const
%token T_string_const
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


/*not all these have to be declared here, we put them at first to remember nonterminals*/

%start program /*func-def header func-decl var-def stmt ttype id formal simple expr simple-list atom call string-literal int-const char-const*/

%type <> program 
/*%type <> func-def
%type <> header
%type <> func-decl
%type <> var-def
%type <> stmt
%type <> ttype
%type <> id
%type <> formal
%type <> simple
%type <> expr
%type <> simple-list
%type <> atom
%type <> call
%type <> string-literal
%type <> int-const
%type <> char-const*/

%%

program	: func_def T_eof	{()}

func_def : T_def header T_colon def multi_stmt T_end 	{()}

def : /*nothing*/ 	{()}
	 | func_def def	{()}
	 | func_decl def	{()}
	 | var_def def	{()}

multi_stmt : stmt	{()}
		   | stmt multi_stmt	{()}

header : mytype id T_lbracket myformal T_rbracket	{()}

mytype : /*nothing*/ 	{()}
	   | ttype	{()}

myformal : /*nothing*/	{()}
		 | formal repformal	{()}

repformal : /*nothing*/	{()}
		  | T_semicolon formal repformal	{()}

formal : myref ttype id other_id	{()}

myref : /*nothing*/	{()}
	  | T_ref	{()}

other_id : /*nothing*/	{()}
		 | T_comma id other_id	{()}

ttype : T_int	{()}
	 | T_bool	{()}
	 | T_char	{()}
	 | ttype T_lsqbracket T_rsqbracket	{()}
	 | T_list T_lsqbracket ttype T_rsqbracket	{()}

func_decl : T_decl header	{()}

var_def : ttype other_id	{()}

stmt : simple	{()}
	 | T_exit	{()}
	 | T_return expr	{()}
	 | T_if expr T_colon multi_stmt elsif_stmt else_stmt T_end	{()}
	 | T_for simple_list T_semicolon expr T_semicolon simple_list T_colon multi_stmt T_end	{()}

elsif_stmt : /*nothing*/	{()}
		  | T_elsif expr T_colon multi_stmt elsif_stmt	{()}

else_stmt : /*nothing*/	{()}
		  | T_else T_colon multi_stmt	{()}

simple : T_skip	{()}
	   | atom T_assign expr	{()}
	   | call	{()}

simple_list : simple other_simple	{()}

other_simple : /*nothing*/	{()}
			 | T_comma simple other_simple	{()}

call : id T_lbracket T_rbracket	{()}
	 | id T_lbracket expr other_expr T_rbracket	{()}

other_expr : /*nothing*/	{()}
		   | T_comma expr other_expr	{()}

atom : id	{()}
	 | T_string_const	{()}
	 | atom T_lsqbracket expr T_rsqbracket	{()}
	 | call	{()}

expr : atom	{()}
	 | T_int_const	{()}
	 | T_char_const	{()}
	 | T_lbracket expr T_rbracket	{()}
	 | T_plus expr %prec NT_plus	{()}
	 | T_minus expr %prec NT_minus	{()}
	 | expr oper expr	{()}
	 | expr lg_oper expr {()}
	 | T_true	{()}
	 | T_false	{()}
	 | T_not expr	{()}
	 | expr T_and expr	{()}
	 | expr T_or expr	{()}
	 | T_new ttype T_lsqbracket expr T_rsqbracket	{()}
	 | T_nil	{()}
	 | T_is_nil T_lbracket expr T_rbracket	{()}
	 | expr T_cons expr	{()}
	 | T_head T_lbracket expr T_rbracket	{()}
	 | T_tail T_lbracket expr T_rbracket	{()}

oper : T_plus {()} | T_minus {()} | T_times {()} | T_div {()} | T_mod {()}
 
lg_oper : T_eq {()} | T_dif {()} | T_less {()} | T_greater {()} | T_less_eq {()} | T_greater_eq {()}

id : T_var {()}

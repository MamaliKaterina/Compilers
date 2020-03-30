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


%left T_right
%left T_and
%nonassoc NT_not
%nonaassoc T_eq T_dif T_less T_greater T_less_eq T_greater_eq
%right T_cons
%left T_plus T_minus
%left T_times T_div T_mod
%nonassoc NT_plus NT_minus


/*not all these have to be declared here, we put them at first to remember nonterminals*/

%start program func-def header func-decl var-def stmt type id formal simple expr simple-list atom call string-literal int-const char-const

%type<> program 
%type<> func-def
%type<> header
%type<> func-decl
%type<> var-def
%type<> stmt
%type<> type
%type<> id
%type<> formal
%type<> simple
%type<> expr
%type<> simple-list
%type<> atom
%type<> call
%type<> string-literal
%type<> int-const
%type<> char-const 

%%
program	: func_def 	{()}
;

func_def : T_def header T_colon def* stmt+ T_end 	{()}
;
def* : /*nothing*/ 	{()}
	| func-def def*	{()}
	| func-decl def*	{()}
	| var-def def*	{()}
;
stmt+ : stmt	{()}
	| stmt stmt+	{()}
;

header : mytype id T_lbracket myformal T_rbracket	{()}
;
mytype : /*nothing*/ 	{()}
	| type	{()}
;
myformal : /*nothing*/	{()}
		| formal repformal	{()}
;
repformal : /*nothing*/	{()}
		| T_semicolon formal repformal	{()}
;

formal : myref type id other-id	{()}
;
myref : /*nothing*/	{()}
	| ref	{()}
;
other-id : /*nothing*/	{()}
		| T_comma id other-id	{()}
;

type : T_int	{()}
	| T_bool	{()}
	| T_char	{()}
	| type T_lsqbracket Trsqbracket	{()}
	| T_list T_lsqbracket type Trsqbracket	{()}
;

(*...*)

expr : atom	{()}
	| int-const	{()}
	| char-const	{()}
	| T_lbracket expr T_rbracket	{()}
	| T_plus expr %prec NT_plus	{()}
	| T_minus expr %prec NT_minus	{()}
	| expr oper expr	{()}
	| expr lg_oper expr {()}
	| T_true	{()}
	| T_false	{()}
	| T_not expr %prec NT_not	{()}
	| expr T_and expr	{()}
	| expr T_or expr	{()}
	| T_new type T_lsqbracket expr T_rsqbracket	{()}
	| T_nil	{()}
	| T_nil? T_lbracket expr T_rbracket	{()}
	| expr T_cons expr	{()}
	| T_head T_lbracket expr T_rbracket	{()}
	| T_tail T_lbracket expr T_rbracket	{()}
;
oper : T_plus {()} | T_minus {()} | T_times {()} | T_div {()} | T_mod {()}
; 
lg_oper : T_eq {()} | T_dif {()} | T_less {()} | T_greater {()} | T_less_eq {()} | T_graeter_eq {()}
;

/*My rule for comment handling. Do nothing if balanced.*/
comment : T_first_com comment T_last_com	{()}
		| /*nothing*/	{()}
; 


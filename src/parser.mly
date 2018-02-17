/* Ocamlyacc parser for TensorFlock */

%{
open ast
%}

%token SEMI COLON LPAREN RPAREN LBRACE RBRACE LBRAC RBRAC COMMA ARROW LANGLE RANGLE
%token DEFINE NOT EQ NEQ LT LEQ GT GEQ AND OR
%token PLUS MINUS TIMES DIVIDE MOD EXPT
%token IF ELSE INT DOUBLE BOOL T

/* ocaml's literals are 'true' and 'false' whereas we want True and False*/ 
%token TRUE FALSE 
%token <int> LITERAL
%token <string> ID FLIT
%token EOF

%start program
%type <Ast.program> program

%right DEFINE
%nonassoc IF THEN ELSE
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%left EXPT
%right NOT NEG


%%

program:
  decls EOF { $1 }

decls:
   /* nothing */ { [] }
 | decls funct { $2 :: $1 }

funct: 
  ftyp fdef { func($1, $2) }

ftyp:
   ID COLON typ SEMI
     { { fname = $1; typ = $3; } }

fdef:
   ID DEF expr SEMI scope  

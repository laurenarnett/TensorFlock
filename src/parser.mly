/* Ocamlyacc parser for TensorFlock */

%{
open ast
%}

%token SEMI COLON COMMA ARROW 
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK LANGLE RANGLE
%token DEFINE NOT EQ NEQ LT LEQ GT GEQ AND OR
%token PLUS MINUS TIMES DIVIDE MOD EXPT
%token IF THEN ELSE INT DOUBLE BOOL TENSOR

%token <int> LITERAL
%token <bool> BLIT
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

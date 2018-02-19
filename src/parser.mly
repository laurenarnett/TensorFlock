/* Ocamlyacc parser for TensorFlock */

%{ open Ast %}

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
   /* empty program */ { [] }
 | decls funct { $2 :: $1 }

funct: 
  ftyp fdef { ($1, $2) }

ftyp:
   ID COLON types SEMI
     { { fname = $1; types = $3; } }

formals:
    { [] }
  | formals ID { $2 :: $1 }

fdef:
   ID formals DEFINE expr SEMI scope  
     { { fname = $1; fargs = List.rev $2; main_expr = $4; scope = $6; } } 

types:
  /* Don't pattern match on empty list because that should fail */
  | typ ARROW types { $1 :: $3 }  
  | typ { [$1] }  

typ:
    INT     { Int   }
  | BOOL    { Bool  }
  | DOUBLE  { Double }
  /* | TENSOR LANGLE shape RANGLE { Tensor($3) } */

expr:
    LITERAL          { Literal($1)            }
  | FLIT	         { Fliteral($1)           }
  | BLIT             { BoolLit($1)            }
  | ID               { Id($1)                 }
  /* Arithmetic ops */
  | expr PLUS   expr { Aop($1, Add,   $3)   }
  | expr MINUS  expr { Aop($1, Sub,   $3)   }
  | expr TIMES  expr { Aop($1, Mult,  $3)   }
  | expr DIVIDE expr { Aop($1, Div,   $3)   }
  | expr MOD    expr { Aop($1, Mod,   $3)   }
  | expr EXPT   expr { Aop($1, Expt,   $3)   }
  /* Relational ops */
  | expr EQ     expr { Rop($1, Eq, $3)   }
  | expr NEQ    expr { Rop($1, Neq,   $3)   }
  | expr LT     expr { Rop($1, LT,  $3)   }
  | expr LEQ    expr { Rop($1, Leq,   $3)   }
  | expr GT     expr { Rop($1, GT, $3) }
  | expr GEQ    expr { Rop($1, Geq,   $3)   }
  /* Boolean ops */  
  | expr AND    expr { Boolop($1, And,   $3)   }
  | expr OR     expr { Boolop($1, Or,    $3)   }
  /* Unary ops */    
  | MINUS expr %prec NEG { Unop(Neg, $2)      }
  | NOT expr         { Unop(Not, $2)          }
  /* Parens */
  | LPAREN expr RPAREN { $2                   }
  /* TODO:Function call */ 
  /* TODO:Brackets */ 

scope:
  /* This only allows us to have scopes with one function in them */ 
  /* TODO: Fix this */
           { [] }
   | LBRACE decls RBRACE { $2 }

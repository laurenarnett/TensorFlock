/* Ocamlyacc parser for TensorFlock */

%{ open Ast %}

%token SEMI COLON COMMA ARROW
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK LANGLE RANGLE
%token DEFINE EQ NEQ LT LEQ GT GEQ AND OR
%token PLUS MINUS TIMES DIVIDE MOD EXPT
%token IF THEN ELSE NAT BOOL TENSOR

%token <int> LITERAL
%token <bool> BLIT
%token <string> ID FLIT TIDX
%token EOF

%start program
%type <Ast.program> program

%right DEFINE
%nonassoc IF THEN ELSE
%left OR
%left AND
%left EQ NEQ
%left LANGLE RANGLE LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right NEG
%right EXPT
%nonassoc LPAREN RPAREN


%%

program:
  decls EOF { List.rev $1 }

decls:
   /* empty program */ { [] }
 | decls funct { $2 :: $1 }

funct:
  ftyp fdef { ($1, $2) }

ftyp:
   ID COLON types SEMI
     { { fname = $1; types = List.rev $3; } }

formals:
    { [] }
  | formals ID { $2 :: $1 }

fdef:
   ID formals DEFINE expr SEMI scope  
     { { fname = $1; fargs = List.rev $2; 
         main_expr = $4; scope = List.rev $6; } } 


types:
  /* Don't pattern match on empty list because that should fail */
  | types ARROW typ { $3 :: $1 }
  | typ { [$1] }

typ:
    NAT     { Nat   }
  | BOOL    { Bool  }
  | DOUBLE  { Double }
  | TENSOR LANGLE shape RANGLE { Tensor($3) }

/* Expression starting point */
expr: binexpr { $1 }

binexpr:
  | IF binexpr THEN binexpr ELSE binexpr { CondExpr($2, $4, $6) }
  | binexpr OR  binexpr     { Binop($1, Or, $3)   }
  | binexpr AND binexpr     { Binop($1, And, $3)  }
  | binexpr EQ  binexpr     { Binop($1, Eq, $3)   }
  | binexpr NEQ binexpr     { Binop($1, Neq, $3)  }
  | binexpr LANGLE  binexpr { Binop($1, LT, $3)   }
  | binexpr LEQ binexpr     { Binop($1, Leq, $3)  }
  | binexpr RANGLE  binexpr { Binop($1, GT, $3)   }
  | binexpr GEQ binexpr     { Binop($1, Geq, $3)  }
  | binexpr PLUS   binexpr  { Binop($1, Add, $3)  }
  | binexpr MINUS  binexpr  { Binop($1, Sub, $3)  }
  | binexpr TIMES  binexpr  { Binop($1, Mult, $3) }
  | binexpr DIVIDE binexpr  { Binop($1, Div, $3)  }
  | binexpr MOD    binexpr  { Binop($1, Mod, $3)  }
  | MINUS binexpr %prec NEG { Unop(Neg, $2)       }
  | binexpr EXPT   binexpr  { Binop($1, Expt, $3) }
  | LPAREN binexpr RPAREN   { $2 }
  | fexpr                   { $1 }

fexpr:
      fexpr brackexpr { App($1, $2) }
  | fexpr LPAREN expr RPAREN { App($1, $3) }
  | brackexpr { $1 }

brackexpr:
    TIDX tidx RBRACK 
      { TensorIdx($1, List.rev $2) }
  | lexpr { $1 }

lexpr:
    LITERAL          { Literal($1)            }
  | FLIT	         { Fliteral($1)           }
  | BLIT             { BoolLit($1)            }
  | ID               { Id($1) }       
  | LBRACK tcontents RBRACK        { TLit(List.rev $2) }

tidx:
  /* Don't match on an empty tensor index */
  | expr            { [$1] }
  | tidx COMMA expr { $3 :: $1 }

scope:
           { [] }
   | LBRACE decls RBRACE { $2 }


tcontents:
       /* We begin the tcontents parsing at the lexpr section so that we only */
       /* admit literal values in tensor literals. Should we be so inclined, */
       /* admitting arbitrary expressions in tensor */
       /* literals can be accomplished by replacing lexpr in */
       /* the following two lines with expr */ 
       lexpr { [$1] }
   | tcontents COMMA lexpr { $3 :: $1 }


shape:
    /* 0 dimensional */     { [] }
  | sexpr COMMA shape   { $1 :: $3 }
  | sexpr               { [$1] }

/* Tensor shape starting point */
sexpr: saexpr { $1 }

saexpr:
      saexpr PLUS saexpr   { Aop($1, Add, $3) : aexpr }
  | saexpr MINUS saexpr    { Aop($1, Sub, $3) }
  | saexpr TIMES saexpr    { Aop($1, Mult, $3) }
  | saexpr DIVIDE saexpr   { Aop($1, Div, $3) }
  | saexpr MOD    saexpr   { Aop($1, Mod, $3) }
  | saexpr EXPT saexpr     { Aop($1, Expt, $3) }
  | MINUS saexpr %prec NEG { Unop(Neg, $2) }
  | sfexpr                 { $1 }

sfexpr:
      sfexpr slexpr { App($1, $2) }
  | slexpr { $1 }


slexpr:
    LITERAL          { Literal($1) }
  | ID               { Id($1)      }


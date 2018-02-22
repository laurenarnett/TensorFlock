/* Ocamlyacc parser for TensorFlock */

%{ open Ast %}

%token SEMI COLON COMMA ARROW
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK LANGLE RANGLE
%token DEFINE NOT EQ NEQ LT LEQ GT GEQ AND OR
%token PLUS MINUS TIMES DIVIDE MOD EXPT
%token IF THEN ELSE INT DOUBLE BOOL TENSOR

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
%left EXPT
%right NOT NEG


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
     { { fname = $1; types = $3; } }

formals:
    { [] }
  | formals ID { $2 :: $1 }

fdef:
   ID formals DEFINE expr SEMI scope  
     { { fname = $1; fargs = List.rev $2; 
         main_expr = $4; scope = List.rev $6; } } 


types:
  /* Don't pattern match on empty list because that should fail */
  | typ ARROW types { $1 :: $3 }
  | typ { [$1] }

tidx:
  /* Don't match on an empty tensor index */
  | expr COMMA tidx { $1 :: $3 }
  | expr            { [$1] }

typ:
    INT     { Int   }
  | BOOL    { Bool  }
  | DOUBLE  { Double }
  /* | TENSOR LANGLE shape RANGLE { Tensor($3) } */

/* Expression starting point */
expr:
    boolexpr { $1 }

boolexpr:
  | IF boolexpr THEN boolexpr ELSE boolexpr { CondExpr($2, $4, $6) }
  | boolexpr OR  boolexpr { Boolop($1, Or, $3) }
  | boolexpr AND boolexpr { Boolop($1, And, $3) }
  | boolexpr EQ  boolexpr { Rop($1, Eq, $3) }
  | boolexpr NEQ boolexpr { Rop($1, Neq, $3) }
  | boolexpr LANGLE  boolexpr { Rop($1, LT, $3) }
  | boolexpr LEQ boolexpr { Rop($1, Leq, $3) }
  | boolexpr RANGLE  boolexpr { Rop($1, GT, $3) }
  | boolexpr GEQ boolexpr { Rop($1, Geq, $3) }
  | aexpr { $1 }

aexpr:
      aexpr PLUS aexpr { Aop($1, Add, $3) }
  | aexpr MINUS aexpr  { Aop($1, Sub, $3) }
  | aexpr TIMES aexpr  { Aop($1, Mult, $3) }
  | aexpr DIVIDE aexpr  { Aop($1, Div, $3) }
  | aexpr MOD    aexpr  { Aop($1, Mod, $3) }
  | aexpr EXPT aexpr { Aop($1, Expt, $3) }
  | uexpr { $1 }

uexpr:
      MINUS uexpr { Unop(Neg, $2) }
  | fexpr { $1 }

fexpr:
      fexpr bexpr { App($1, $2) }
  | bexpr { $1 }

bexpr:
    TIDX tidx RBRACK 
      { let id = List.hd (String.split_on_char '[' $1) in TensorIdx(id, $2) }
  | lexpr { $1 }

lexpr:
    LITERAL          { Literal($1)            }
  | FLIT	         { Fliteral($1)           }
  | BLIT             { BoolLit($1)            }
  | ID               { Id($1) }       
  | tcontents        { TLit($1) }

scope:
           { [] }
   | LBRACE decls RBRACE { $2 }


tcontents:
   LBRACK floats RBRACK { $2 } 
   
floats:
       { [] }
   | FLIT              { [$1] } 
   | FLIT COMMA floats { $1 :: $3 } 

shape:
    /* 0 dimensional */     { [] }
  | shape_arg COMMA shape   { $1 :: $3 }
  | shape_arg               { [$1] }

shape_arg:
    LITERAL                     { Int($1) }
  | ID                          { Placeholder($1) }
  | shape_arg PLUS   shape_arg { Poly($1, Add,   $3)   }
  | shape_arg MINUS  shape_arg { Poly($1, Sub,   $3)   }
  | shape_arg TIMES  shape_arg { Poly($1, Mult,  $3)   }
  | shape_arg DIVIDE shape_arg { Poly($1, Div,   $3)   }
  | shape_arg MOD    shape_arg { Poly($1, Mod,   $3)   }
  | shape_arg EXPT   shape_arg { Poly($1, Expt,   $3)   } 

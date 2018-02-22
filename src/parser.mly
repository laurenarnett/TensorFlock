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

/* %right DEFINE */
/* %nonassoc IF THEN ELSE */
/* %left OR */
/* %left AND */
/* %left EQ NEQ */
/* %left LT GT LEQ GEQ */
/* %left PLUS MINUS */
/* %left TIMES DIVIDE MOD */
/* %left EXPT */
/* %right NOT NEG */


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

tidx:
  /* Don't match on an empty tensor index */
  | expr COMMA tidx { $1 :: $3 }
  | expr            { [$1] }

typ:
    INT     { Int   }
  | BOOL    { Bool  }
  | DOUBLE  { Double }
  /* | TENSOR LANGLE shape RANGLE { Tensor($3) } */

expr:
  | IF expr THEN expr ELSE expr { CondExpr($2, $4, $6) }
  | orexpr { $1 }  

orexpr:
      orexpr OR andexpr { Boolop($1, Or, $3) }
  | andexpr { $1 }

      
andexpr:
      andexpr AND eqexpr { Boolop($1, And, $3) }
  | eqexpr { $1 }

eqexpr:
      eqexpr EQ rexpr { Rop($1, Eq, $3) }
  | eqexpr NEQ rexpr { Rop($1, Neq, $3) }

rexpr:
      rexpr LT aexpr { Rop($1, LT, $3) }
  | rexpr LEQ aexpr  { Rop($1, Leq, $3) }
  | rexpr GT aexpr  { Rop($1, GT, $3) }
  | rexpr GEQ aexpr  { Rop($1, Geq, $3) }
  | aexpr { $1 }

aexpr:
      aexpr PLUS mexpr { Aop($1, Add, $3) }
  | aexpr MINUS mexpr  { Aop($1, Sub, $3) }
  | mexpr { $1 }

mexpr:
      mexpr TIMES eexpr { Aop($1, Mult, $3) }
  | mexpr DIVIDE eexpr  { Aop($1, Div, $3) }
  | mexpr MOD    eexpr  { Aop($1, Mod, $3) }
  | eexpr { $1 }

eexpr:
      eexpr EXPT uexpr { Aop($1, Expt, $3) }
  | uexpr { $1 }

uexpr:
      MINUS fexpr { Unop(Neg, $2) }
  | NOT fexpr { Unop(Not, $2) }
  | fexpr { $1 }

fexpr:
      fexpr bexpr { App($1, $2) }
  | bexpr { $1 }

bexpr:
      /* ID LBRACK tidx RBRACK { TensorIdx($1, $3) } */
      TIDX tidx RBRACK { let id = List.hd (String.split_on_char '[' $1) 
                         in TensorIdx(id, $2) }
  | lexpr { $1 }

lexpr:
    LITERAL          { Literal($1)            }
  | FLIT	         { Fliteral($1)           }
  | BLIT             { BoolLit($1)            }
  | ID               { Id($1)                 }
  | tcontents        { TLit($1) }

/* expr: */
/*   /1* Arithmetic ops *1/ */
/*   | expr PLUS   expr { Aop($1, Add,   $3)   } */
/*   | expr MINUS  expr { Aop($1, Sub,   $3)   } */
/*   | expr TIMES  expr { Aop($1, Mult,  $3)   } */
/*   | expr DIVIDE expr { Aop($1, Div,   $3)   } */
/*   | expr MOD    expr { Aop($1, Mod,   $3)   } */
/*   | expr EXPT   expr { Aop($1, Expt,   $3)   } */ 
/*   /1* Relational ops *1/ */
/*   | expr EQ     expr { Rop($1, Eq, $3)   } */
/*   | expr NEQ    expr { Rop($1, Neq,   $3)   } */
/*   | expr LT     expr { Rop($1, LT,  $3)   } */
/*   | expr LEQ    expr { Rop($1, Leq,   $3)   } */
/*   | expr GT     expr { Rop($1, GT, $3) } */
/*   | expr GEQ    expr { Rop($1, Geq,   $3)   } */ 
/*   /1* Unary ops *1/ */
/*   | MINUS expr /1* %prec NEG *1/ { Unop(Neg, $2)      } */
/*   | NOT expr         { Unop(Not, $2)          } */
/*   /1* Parens *1/ */
/*   | LPAREN expr RPAREN { $2 } */
/*   /1* Conditional Expressions *1/ */
/*   | IF expr THEN expr ELSE expr { CondExpr($2, $4, $6) } */
/*   /1* Bracket indexing *1/ */
/*   | ID LBRACK tidx RBRACK { TensorIdx($1, $3) } */
/*   /1* Function application *1/ */
/*   /1* | fexpr { $1 } *1/ */
/*   /1* Literals *1/ */  
/*   | lit { $1 } */

/* fexpr: */
/*   | expr expr { App($1, $2) } */
  
/* lit: */
/*     LITERAL          { Literal($1)            } */
/*   | FLIT	         { Fliteral($1)           } */
/*   | BLIT             { BoolLit($1)            } */
/*   | ID               { Id($1)                 } */
/*   | tcontents        { TLit($1) } */

scope:
           { [] }
   | LBRACE decls RBRACE { $2 }


tcontents:
   LBRACK floats RBRACK { $2 } 
   
floats:
       { [] }
   | FLIT              { [$1] } 
   | FLIT COMMA floats { $1 :: $3 } 

/* shape: */
/*     /1* 0 dimensional *1/     { [] } */
/*   | shape_arg COMMA shape   { $1 :: $3 } */
/*   | shape_arg               { [$1] } */

/* shape_arg: */
/*     LITERAL                     { Int } */
/*   | ID                          { Placeholder } */
/*   | shape_arg PLUS   shape_arg { Poly($1, Add,   $3)   } */
/*   | shape_arg MINUS  shape_arg { Poly($1, Sub,   $3)   } */
/*   | shape_arg TIMES  shape_arg { Poly($1, Mult,  $3)   } */
/*   | shape_arg DIVIDE shape_arg { Poly($1, Div,   $3)   } */
/*   | shape_arg MOD    shape_arg { Poly($1, Mod,   $3)   } */
/*   | shape_arg EXPT   shape_arg { Poly($1, Expt,   $3)   } */ 

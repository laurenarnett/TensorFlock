/* Ocamlyacc parser for TensorFlock */

%{ open Ast %}

%token SEMI COLON COMMA ARROW
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK LANGLE RANGLE
%token DEFINE EQ NEQ LEQ GEQ AND OR
%token PLUS MINUS TIMES DIVIDE MOD EXPT
%token IF THEN ELSE NAT BOOL TENSOR MAIN

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
  main decls EOF { ($1, List.rev $2) }

main:
  MAIN DEFINE expr SEMI {  $3 }

decls:
   /* empty program */ { [] }
 | decls funct { $2 :: $1 }

funct:
  ftyp fdef
    { if $1.ftyp_name <> (String.sub $2.fdef_name 0 (String.length $1.ftyp_name))
      then raise
      (Failure ("\nName of function in type: " ^ $1.ftyp_name ^
                "\nand in definition: " ^ $2.fdef_name ^ "\ndo not match\n"))
      else ($1, $2) }

ftyp:
   ID COLON types SEMI
     { { ftyp_name = $1; types = $3; } }

lhsid:
    ID { $1 }
  | TIDX tidx RBRACK { $1 ^ "[" ^ String.concat "," $2 ^ "]"}

formals:
    { [] }
  | formals lhsid { $2 :: $1 }

fdef:
   lhsid formals DEFINE expr SEMI scope
     { { fdef_name = $1; fparams = List.rev $2;
         main_expr = $4; scope   = List.rev $6; } }

types:
  /* Don't pattern match on empty list because that should fail */
  | typ ARROW types { Arrow($1, $3) }
  | typ { $1 }

typ:
    NAT     { Nat   }
  | BOOL    { Bool  }
  | TENSOR LANGLE shape RANGLE { Tensor($3) }

/* Expression starting point */
expr: binexpr { $1 }

binexpr:
  | IF binexpr THEN binexpr ELSE binexpr { CondExpr($2, $4, $6) }
  | binexpr OR  binexpr     { Boolop($1, Or, $3)   }
  | binexpr AND binexpr     { Boolop($1, And, $3)  }
  | binexpr EQ  binexpr     { Rop($1, Eq, $3)   }
  | binexpr NEQ binexpr     { Rop($1, Neq, $3)  }
  | binexpr LANGLE  binexpr { Rop($1, LT, $3)   }
  | binexpr LEQ binexpr     { Rop($1, Leq, $3)  }
  | binexpr RANGLE  binexpr { Rop($1, GT, $3)   }
  | binexpr GEQ binexpr     { Rop($1, Geq, $3)  }
  | binexpr PLUS   binexpr  { Aop($1, Add, $3)  }
  | binexpr MINUS  binexpr  { Aop($1, Sub, $3)  }
  | binexpr TIMES  binexpr  { Aop($1, Mult, $3) }
  | binexpr DIVIDE binexpr  { Aop($1, Div, $3)  }
  | binexpr MOD    binexpr  { Aop($1, Mod, $3)  }
  | MINUS binexpr %prec NEG { Unop(Neg, $2)       }
  | binexpr EXPT   binexpr  { Aop($1, Expt, $3) }
  | LPAREN binexpr RPAREN   { $2 }
  | fexpr                   { $1 }

fexpr:
      fexpr brackexpr { App($1, $2) }
  | fexpr LPAREN expr RPAREN { App($1, $3) }
  | brackexpr { $1 }

brackexpr:
    TIDX tidx RBRACK
      { TensorIdx(Id $1, List.rev $2) }
  | lexpr { $1 }

lexpr:
    LITERAL          { Literal($1)            }
  | FLIT	         { Fliteral($1)           }
  | BLIT             { BoolLit($1)            }
  | ID               { Id($1) }
  | LBRACK tcontents RBRACK        { TLit(List.rev $2) }

tidx:
  /* Don't match on an empty tensor index */
  | ID            { [$1] }
  | tidx COMMA ID { $3 :: $1 }

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
   | tcontents lexpr { $2 :: $1 }


shape:
    /* 0 dimensional */     { [] }
  | sexpr COMMA shape   { $1 :: $3 }
  | sexpr               { [$1] }

/* Tensor shape starting point */
sexpr: saexpr { $1 }

saexpr:
      saexpr PLUS saexpr   { AAop($1, Add, $3) }
  | saexpr MINUS saexpr    { AAop($1, Sub, $3) }
  | saexpr TIMES saexpr    { AAop($1, Mult, $3) }
  | saexpr DIVIDE saexpr   { AAop($1, Div, $3) }
  | saexpr MOD    saexpr   { AAop($1, Mod, $3) }
  | saexpr EXPT saexpr     { AAop($1, Expt, $3) }
  | slexpr                 { $1 }

slexpr:
    LITERAL          { ALiteral($1) }
  | ID               { AId($1)      }


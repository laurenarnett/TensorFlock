open Ast

type sexpr = typ * sexpr_detail
and sexpr_detail = 
    SLiteral of int
  | SFliteral of string
  | SBoolLit of bool
  | STLit of shape * string list 
  | SId of string
  | SUnop of uop * sexpr
  | SBinop of sexpr * binop * sexpr
  | SApp of sexpr * sexpr
  | SCondExpr of sexpr * sexpr * sexpr
  | STensorIdx of shape * string * sexpr list

type sfunc = {
    sfname : string;
    stype : typ list;
    sfargs : string list;
    sfexpr : sexpr;
    sscope : sfunc list;
}

type sfunc = sfunc_type * sfunc_def
type sprogram = sexpr * sfunc list

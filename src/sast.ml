open Ast

type saexpr = typ * saexpr_detail
and saexpr_detail = 
    SLiteral of int
  | SId of string
  | SAop of saexpr * aop * saexpr
  | SUnop of uop * saexpr
  | SApp of saexpr * saexpr

type sexpr = typ * sexpr_detail
and sexpr_detail = 
    SLiteral of int
  | SFliteral of string
  | SBoolLit of bool
  | STLit of sexpr list
  | SId of string
  | SUnop of uop * sexpr
  | SBinop of sexpr * binop * sexpr
  | SApp of sexpr * sexpr
  | SCondExpr of sexpr * sexpr * sexpr
  | STensorIdx of string * sexpr list

type sfunc_type = {
  sfname : string;
  stypes : typ list
}

type sfunc_def = {
  sfname : string;
  sfargs : string list;
  smain_expr : expr;
  sscope : (sfunc_type * sfunc_def) list;
}

type sfunc = sfunc_type * sfunc_def
type sprogram = sfunc list

(* Abstract syntax tree *)

(* arithmetic operations of type aop : Double -> Double -> Double *)
(* OR Int -> Int -> Int *)
type aop = Add | Sub | Mult | Div | Mod | Expt

(* relational operators of type rop : Double -> Double -> Bool *)
type rop = Eq | Neq | LT | Leq | GT | Geq

(* boolean operators of type bop : Bool -> Bool -> Bool *)
type bop = And | Or

(* unary operators *)
type uop = Not | Neg

(* tensor shape arguments TODO: come up with better names *)
type shape_arg = Placeholder | Int | Poly of shape_arg * aop * shape_arg

(* the shape of a tensor is a list of shapeargs *)
type shape = shape_arg list

(* types: TODO decide if we support Ints or just Naturals (i.e) unsigned ints *)
type typ = Bool | Int | Double | Tensor of shape

type expr = 
    Literal of int
  | Fliteral of string
  | BoolLit of bool
  | Id of string
  | Aop of expr * aop * expr
  | Unop of uop * expr
  | Boolop of expr * bop * expr  
  | Call of string * expr list
  | CondExpr of expr * expr * expr 

type func_type = {
  fname : string;
  types : typ list;  
}

type func_def = {
  main_expr : expr;
  scope : func_type * func_def list;
}

type func = func_type * func_def
type program = func list

(* Pretty printing *) 
let string_of_aop = function
    Add  -> "+"
  | Sub  -> "-"
  | Mult -> "*"
  | Div  -> "/"
  | Mod  -> "%"
  | Expt -> "^"

let string_of_rop = function
    Eq  -> "=="
  | Neq -> "!="
  | LT  -> "<"
  | GT  -> ">"
  | Leq -> "<="
  | Geq -> ">="

let string_of_bop = function
    And -> "&&"
  | Or  -> "||"

let string_of_uop = function
    Not  -> "!"
  | Neg  -> "-"

(* Abstract syntax tree *)

(* arithmetic operations of type aop : Double -> Double -> Double *)
(* OR Int -> Int -> Int *)
type aop = Add | Sub | Mult | Div | Mod | Exp

(* relational operators of type rop : Double -> Double -> Bool *)
type rop = Eq | Neq | Less | Leq | Greater | Geq

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
  

type fun_type = {
  fname : string;
  typ : typ list;  
}

type fun_def = {
  body : expr list;
}

type func = fun_type * fun_def
type program = func list

(* Abstract syntax tree *)

(* arithmetic operations of type aop : Double -> Double -> Double *)
type aop = Add | Sub | Mult | Div | Mod | Exp

(* relational operators of type rop : Double -> Double -> Bool *)
type rop = Eq | Neq | Less | Leq | Greater | Geq

(* boolean operators of type bop : Bool -> Bool -> Bool *)
type bop = And | Or

(* unary operators *)
type uop = Not | Neg

(* types: TODO decide if we support Ints or just Naturals (i.e) unsigned ints *)
type typ = Bool | Nat | Double | Tensor

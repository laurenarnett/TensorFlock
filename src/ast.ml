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
  | Rop of expr * rop * expr
  | Call of string * expr list
  | CondExpr of expr * expr * expr 

type func_type = {
  fname : string;
  types : typ list;  
}

type func_def = {
  fname : string;
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

let string_of_expr = function
    Literal(l) -> string_of_int l
  | Fliteral(l) -> l
  | BoolLit(True) -> "True"
  | BoolLit(False) -> "False"
  | Id(s) -> s
  | Aop(e1, o, e2) ->
        string_of_expr e1 ^ " " ^ string_of_aop o ^ " " ^ string_of_expr e2
  | Unop(o, e) ->
        string_of_uop o ^ string_of_expr e 
  | Boolop(e1, o , e2) ->
        string_of_expr e1 ^ " " ^ string_of_bop o ^ " " ^ string_of_expr e2
  | Rop(e1, o, e2) ->
        string_of_expr e1 ^ " " ^ string_of_rop o ^ " " ^ string_of_expr e2
  | Call(f, el) ->
        f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | CondExpr(e1, e2, e3) ->
        "if " ^ string_of_expr e1 ^ " then " ^ string_of_expr e2 ^ " else " ^
        string_of_expr e3

let string_of_typ = function
    Bool -> "bool"
  | Int -> "int"
  | Double -> "double"

let rec string_of_func_type ftype = 
    ftype.fname ^ " : " ^ String.concat " -> " (List.map string_of_typ
    ftype.types) ^ " \n"

    and string_of_scope scope = 
        "{" ^ List.map (fun (ft, fd) -> string_of_func_type ft ^ 
        string_of_func_def fd) scope ^ "}" 

    and string_of_func_def fdef = 
        fdef.fname ^ " = " ^ string_of_expr fdef.main_expr ^ string_of_scope
        fdef.scope ^ " \n" in

let string_of_func (ft, fd) = 
    string_of_func_type ft ^ string_of_func_def fd

let string_of_program funcs = 
    List.map string_of_func funcs

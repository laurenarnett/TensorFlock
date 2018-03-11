(* Abstract syntax tree *)

(* arithmetic operations of type aop : Double -> Double -> Double *)
(* OR Int -> Int -> Int *)
type aop = Add | Sub | Mult | Div | Mod | Expt

(* relational operators of type rop : Double -> Double -> Bool *)
type rop = Eq | Neq | LT | Leq | GT | Geq

(* boolean operators of type bop : Bool -> Bool -> Bool *)
type bop = And | Or

type binop = Add | Sub | Mult | Div | Mod | Expt |
             Eq  | Neq | LT | Leq | GT | Geq |
             And | Or

(* unary operators *)
type uop = Neg

(* restricted class of expressions that can go in tensor shapes *)
type aexpr = 
    Literal of int
  | Id of string
  | Aop of aexpr * aop * aexpr
  | Unop of uop * aexpr
  | App of aexpr * aexpr


type shape = aexpr list
type typ = Bool | Nat | Tensor of shape

type expr =
    Literal of int
  | Fliteral of string
  | BoolLit of bool
  | TLit of expr list
  | Id of string
  | Unop of uop * expr
  | Binop of expr * binop * expr
  (* | Aop of expr * aop * expr *)
  (* | Boolop of expr * bop * expr *)
  (* | Rop of expr * rop * expr *)
  | App of expr * expr
  | CondExpr of expr * expr * expr 
  | TensorIdx of string * expr list

type func_type = {
  ftyp_name : string;
  types : typ list;
}

type func_def = {
  fdef_name : string;
  fargs : string list;
  main_expr : expr;
  scope : (func_type * func_def) list;
}

type func = func_type * func_def
type program = expr * func list

(* Pretty printing *)
let (string_of_aop : aop -> string) = function
    Add  -> "+"
  | Sub  -> "-"
  | Mult -> "*"
  | Div  -> "/"
  | Mod  -> "%"
  | Expt -> "^"

let (string_of_rop : rop -> string)= function
    Eq  -> "=="
  | Neq -> "!="
  | LT  -> "<"
  | GT  -> ">"
  | Leq -> "<="
  | Geq -> ">="

let (string_of_bop : bop -> string) = function
    And -> "&&"
  | Or  -> "||"

let string_of_binop = function
    Add  -> "+"
  | Sub  -> "-"
  | Mult -> "*"
  | Div  -> "/"
  | Mod  -> "%"
  | Expt -> "^"
  | Eq  -> "=="
  | Neq -> "!="
  | LT  -> "<"
  | GT  -> ">"
  | Leq -> "<="
  | Geq -> ">="
  | And -> "&&"
  | Or  -> "||"

let string_of_uop = function
    Neg  -> "-"

let rec (string_of_aexpr : aexpr -> string) = function
    Literal(l) -> "(" ^ string_of_int l ^ ")"
  | Id(s) -> s
  | Aop(e1, o, e2) ->
        "(" ^ string_of_aexpr e1 ^ " " ^ string_of_aop o ^ " " 
            ^ string_of_aexpr e2 ^ ")"
  | Unop(o, e) ->
        "(" ^ string_of_uop o ^ string_of_aexpr e ^ ")"
  | App(e1, e2) ->
        "(" ^ string_of_aexpr e1 ^ " applied to " ^ string_of_aexpr e2 ^ ")"


let rec string_of_expr = function
    Literal(l) -> "(" ^ string_of_int l ^ ")"
  | Fliteral(l) -> "(" ^ l ^ ")"
  | BoolLit(true) -> "True"
  | BoolLit(false) -> "False"
  | TLit(es) -> "[" ^ String.concat ", " (List.map string_of_expr es) ^ "]"
  | Id(s) -> s
  | Unop(o, e) ->
        "(" ^ string_of_uop o ^ string_of_expr e ^ ")"
  (* | Aop(e1, o, e2) -> *)
  (*       "(" ^ string_of_expr e1 ^ " " ^ string_of_aop o ^ " " *) 
  (*           ^ string_of_expr e2 ^ ")" *)
  (* | Boolop(e1, o , e2) -> *)
  (*       "(" ^ string_of_expr e1 ^ " " ^ string_of_bop o ^ " " ^ string_of_expr *)
  (*         e2 ^ ")" *)
  (* | Rop(e1, o, e2) -> *)
  (*       "(" ^ string_of_expr e1 ^ " " ^ string_of_rop o ^ " " ^ string_of_expr *)
  (*         e2 ^ ")" *)
  | Binop(e1, o, e2) ->
        "(" ^ string_of_expr e1 ^ " " ^ string_of_binop o ^ " " 
            ^ string_of_expr e2 ^ ")"
  | App(e1, e2) ->
        "(" ^ string_of_expr e1 ^ " applied to " ^ string_of_expr e2 ^ ")"
  | CondExpr(e1, e2, e3) ->
        "(" ^ " if " ^ string_of_expr e1 ^ " then " ^ string_of_expr e2 
            ^ " else " ^ string_of_expr e3 ^ ")"
  | TensorIdx(id, idxs) ->
        "(" ^ id ^ "[" ^ String.concat ", " 
            (List.map string_of_expr idxs) ^ "]" ^ ")"


let string_of_typ = function
    Bool -> "Bool"
  | Nat -> "Nat"
  | Tensor s -> "T<" ^ String.concat ", " (List.map string_of_aexpr s) ^ ">"

let rec string_of_func_type (ftype : func_type) =
    ftype.ftyp_name ^ " : " ^ String.concat " -> " (List.map string_of_typ
    ftype.types) ^ ";\n"


    and string_of_scope scope = match scope with
      []  -> ""
      | _ -> "{" ^ String.concat "\n"
                (List.map (fun (ft, fd) -> string_of_func_type ft ^
        string_of_func_def fd) scope) ^ "}"

    and string_of_func_def (fdef : func_def) = 
      fdef.fdef_name ^ " " ^ String.concat " " (fdef.fargs) ^ " = " ^ 
      string_of_expr fdef.main_expr ^ "; " ^ string_of_scope fdef.scope ^ " \n"

let string_of_func (ft, fd) =
    string_of_func_type ft ^ string_of_func_def fd

let string_of_program (main_expr, funcs) =
    "main = " ^ string_of_expr main_expr ^ "\n" ^ 
    String.concat "\n" @@ List.map string_of_func funcs

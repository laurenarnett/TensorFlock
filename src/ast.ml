(* Abstract syntax tree *)

(* arithmetic operations of type aop : Double -> Double -> Double *)
(* OR Int -> Int -> Int *)
type aop = Add | Sub | Mult | Div | Mod | Expt

(* relational operators of type rop : Double -> Double -> Bool *)
type rop = Eq | Neq | LT | Leq | GT | Geq

(* boolean operators of type bop : Bool -> Bool -> Bool *)
type bop = And | Or

(* unary operators *)
type uop = Neg

(* restricted class of expressions that can go in tensor shapes *)
type aexpr =
    ALiteral of int
  | AId of string
  | AAop of aexpr * aop * aexpr


type shape = aexpr list
type typ = Nat | Bool | Tensor of shape | Arrow of typ * typ

type expr =
    Literal of int
  | Fliteral of string
  | BoolLit of bool
  | TLit of expr list
  | Id of string
  | Unop of uop * expr
  | Aop of expr * aop * expr
  | Boolop of expr * bop * expr
  | Rop of expr * rop * expr
  | App of expr * expr
  | CondExpr of expr * expr * expr
  | TensorIdx of expr * string list

type func_type = {
  ftyp_name : string;
  types : typ;
}

type func_def = {
  fdef_name : string;
  fparams : string list;
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

let string_of_uop = function
    Neg  -> "-"

let rec (string_of_aexpr : aexpr -> string) = function
    ALiteral(l) -> "(" ^ string_of_int l ^ ")"
  | AId(s) -> s
  | AAop(e1, o, e2) ->
        "(" ^ string_of_aexpr e1 ^ " " ^ string_of_aop o ^ " "
            ^ string_of_aexpr e2 ^ ")"

let rec (string_of_expr : expr -> string) = function
    Literal(l) -> "(" ^ string_of_int l ^ ")"
  | Fliteral(l) -> "(" ^ l ^ ")"
  | BoolLit(true) -> "True"
  | BoolLit(false) -> "False"
  | TLit(es) -> "[" ^ String.concat ", " (List.map string_of_expr es) ^ "]"
  | Id(s) -> s
  | Unop(o, e) ->
        "(" ^ string_of_uop o ^ string_of_expr e ^ ")"
  | Aop(e1, o, e2) ->
        "(" ^ string_of_expr e1 ^ " " ^ string_of_aop o ^ " "
            ^ string_of_expr e2 ^ ")"
  | Boolop(e1, o , e2) ->
        "(" ^ string_of_expr e1 ^ " " ^ string_of_bop o ^ " " ^ string_of_expr
          e2 ^ ")"
  | Rop(e1, o, e2) ->
        "(" ^ string_of_expr e1 ^ " " ^ string_of_rop o ^ " " ^ string_of_expr
          e2 ^ ")"
  | App(e1, e2) ->
        "(" ^ string_of_expr e1 ^ " applied to " ^ string_of_expr e2 ^ ")"
  | CondExpr(e1, e2, e3) ->
        "(" ^ " if " ^ string_of_expr e1 ^ " then " ^ string_of_expr e2
            ^ " else " ^ string_of_expr e3 ^ ")"
  | TensorIdx(e, idxs) ->
        "(" ^ string_of_expr e ^ "[" ^ String.concat ", " (idxs) ^ "]" ^ ")"


let rec string_of_typ = function
    Bool -> "Bool"
  | Nat -> "Nat"
  | Tensor s -> "T<" ^ String.concat ", " (List.map string_of_aexpr s) ^ ">"
  | Arrow(t, ts) -> string_of_typ t ^ " -> " ^ string_of_typ ts

let rec string_of_func_type (ftype : func_type) =
    ftype.ftyp_name ^ " : " ^ string_of_typ ftype.types ^ ";\n"
    and string_of_scope scope = match scope with
      []  -> ""
      | _ -> "{" ^ String.concat "\n"
                (List.map (fun (ft, fd) -> string_of_func_type ft ^
        string_of_func_def fd) scope) ^ "}"

    and string_of_func_def (fdef : func_def) =
      fdef.fdef_name ^ " " ^ String.concat " " (fdef.fparams) ^ " = " ^
      string_of_expr fdef.main_expr ^ "; " ^ string_of_scope fdef.scope ^ " \n"

let string_of_func (ft, fd) =
    string_of_func_type ft ^ string_of_func_def fd

let string_of_program (main_expr, funcs) =
    "main = " ^ string_of_expr main_expr ^ "\n" ^
    String.concat "\n" @@ List.map string_of_func funcs

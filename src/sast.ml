open Ast

type sexpr = typ * sexpr_detail
and sexpr_detail =
    SLiteral of int
  | SBoolLit of bool
  | SFliteral of string
  (* These are strings because SFLiterals are strings *)
  | STLit of string list * int list
  | SId of string
  | SUnop of uop * sexpr
  | SAop of sexpr * aop * sexpr
  | SBoolop of sexpr * bop * sexpr
  | SRop of sexpr * rop * sexpr
  | SApp of sexpr * sexpr list
  | SCondExpr of sexpr * sexpr * sexpr
  | STensorIdx of shape * string * sexpr list

type sfunc = {
    sfname : string;
    stype : typ;
    sfparams : (typ * string) list;
    sindices : (aexpr * string) list;
    slocals : (typ * string) list;
    sfexpr : sexpr;
    sscope : sfunc list;
}

type sprogram = sexpr * sfunc list

(* Pretty printing *)
let rec string_of_sexpr_detail e = match e with
    | SLiteral(i) -> string_of_int i
    | SFliteral(s) -> s
    | STLit(contents, _shape) -> "[" ^ String.concat " " contents ^ "]" 
    | SId(s) -> s
    | SBoolLit(true) -> "True" | SBoolLit(false) -> "False"
    | SUnop(Neg, sexpr) -> "-" ^ string_of_sexpr sexpr
    | SAop(sexpr1, op, sexpr2) ->
      string_of_sexpr sexpr1 ^ " " ^ string_of_aop op ^ " " ^
      string_of_sexpr sexpr2
    | SBoolop(sexpr1, op, sexpr2) ->
      string_of_sexpr sexpr1 ^ " " ^ string_of_bop op ^ " " ^
      string_of_sexpr sexpr2
    | SRop(sexpr1, op, sexpr2) ->
      string_of_sexpr sexpr1 ^ " " ^ string_of_rop op ^ " " ^
      string_of_sexpr sexpr2
    | SApp(sexpr1, sexprs) ->
        string_of_sexpr sexpr1 ^ "[ " ^ 
        String.concat "," (List.map string_of_sexpr sexprs) ^ "]"
    | SCondExpr(sexpr1, sexpr2, sexpr3) ->
      "if " ^ string_of_sexpr sexpr1 ^ " then " ^ string_of_sexpr sexpr2
      ^ " else " ^ string_of_sexpr sexpr3
    | STensorIdx(_,_,_) -> "Not yet implemented"
and string_of_sexpr (t, det) =
  string_of_sexpr_detail det ^ " : " ^ string_of_typ t

let rec string_of_sfunc sfunc =
  "(" ^ sfunc.sfname ^ (String.concat " " (List.map snd sfunc.sfparams))
  ^ " : " ^ string_of_typ sfunc.stype ^ ") = "
  ^ string_of_sexpr sfunc.sfexpr ^ "\n{\n"
  ^ String.concat "\n" (List.map string_of_sfunc sfunc.sscope)
  ^ "\n}"

let string_of_sprogram (main_expr, sfuncs) =
  "main = " ^ string_of_sexpr main_expr ^ "\n"
  ^ String.concat "\n" @@ List.map string_of_sfunc sfuncs

open Ast

(* Internal representation of types *)
type styp = 
      SBool 
    | SNat of aexpr option
    | SReal
    (* Instead of Tensor(shape), we'll desugar to 
     * SArrow(SIndices, SReal) *)
    | SIndices of shape
    | SArrow of styp * styp

type sexpr = styp * sexpr_detail
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
  | SIndexedTensor of sexpr * index_var list 
  | SIxVar of index_var
and index_var = string * aexpr
(* This is the type of a definition site: 
 * Every ID that is not a tensor index needs to be bound to the location at
 * which it was defined. All IDs are defined in an sfunc. The int tells us
 * which parameter the ID corresponds to. 0 corresponds to the function name.
 * 1 is the first argument of the function, etc. *)
and def_site = sfunc * int

and param = Indices of index_var list | ArgList of string list

and sfunc = {
    sfname : string;
    stype : typ;
    sfparams : param;
    sfexpr : sexpr;
    sscope : sfunc list;
}

type sprogram = sexpr * sfunc list


(* Pretty printing *)
let rec string_of_styp = function
    | SBool -> "SBool"
    | SNat(s) -> 
      (match s with None -> "SNat" | Some s -> "SNat : " ^ string_of_aexpr s)
    | SReal -> "SReal"
    | SIndices(shape) -> 
      "[" ^ String.concat "," (List.map string_of_aexpr shape) ^ "]"
    | SArrow(t1, t2) -> 
      "(" ^ string_of_styp t1 ^ " -> " ^ string_of_styp t2 ^ ")"

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
      string_of_sexpr sexpr1 ^ " " ^ 
      String.concat " " @@ List.map string_of_sexpr sexprs
    | SCondExpr(sexpr1, sexpr2, sexpr3) ->
      "if " ^ string_of_sexpr sexpr1 ^ " then " ^ string_of_sexpr sexpr2
      ^ " else " ^ string_of_sexpr sexpr3
    | SIndexedTensor(tensor_name, indices) -> 
      string_of_sexpr tensor_name ^ "[" 
      ^ (String.concat "," @@ 
      List.map (fun ix -> string_of_aexpr (snd ix)) indices) ^ "]"
    | SIxVar(s, aexpr) -> "(" ^ s ^ " : " ^ string_of_aexpr aexpr ^ " )"
and string_of_sexpr (t, det) =
  string_of_sexpr_detail det ^ " : " ^ string_of_styp t

let rec string_of_sfunc sfunc =
    let string_of_param = function 
        | Indices(ivar_list) -> let strs = List.map fst ivar_list in
            "[" ^ String.concat "," strs ^ "]"
        | ArgList(s) -> String.concat " " s in

  "(" ^ sfunc.sfname ^ 
  (string_of_param sfunc.sfparams)
  ^ " : " ^ string_of_typ sfunc.stype ^ ") = "
  ^ string_of_sexpr sfunc.sfexpr ^ "\n{\n"
  ^ String.concat "\n" (List.map string_of_sfunc sfunc.sscope)
  ^ "\n}"

let string_of_sprogram (main_expr, sfuncs) =
  "main = " ^ string_of_sexpr main_expr ^ "\n"
  ^ String.concat "\n" @@ List.map string_of_sfunc sfuncs

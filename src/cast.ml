open Ast
open Sast
(* Between semant checking and codegen, run one more compiler pass to simplify
 * indexing *)

(* size and shape (shape is only for printing metadata) *)
type ctyp = CBool | CNat | CDouble | CTensor of int * int list

type cexpr = ctyp * cx 
and cx = 
  | CLiteral of int
  | CBoollit of bool
  | CFliteral of string
  | CTlit of string list * int 
  | CId of string
  | CUnop of uop * cexpr
  | CAop of cexpr * aop * cexpr
  | CBoolop of cexpr * bop * cexpr
  | CRop of cexpr * rop * cexpr
  | CCondExpr of cexpr * cexpr * cexpr
  (* By here, we will have resolved all indexing into calculating offsets of
   * single dimension arrays *)
  | CTensorIdx of int * cexpr
  | CApp of cexpr * cexpr list

type assign = {
    name : string;
    typ : ctyp;
    index : int option; (* None means this is a normal variable *)
    cexpr : cexpr;
}

type cfunc = {
    name : string;
    ret_typ : ctyp;
    params : (ctyp * string) list;
    locals : assign list;
    cexpr : cexpr;
}

let rec string_of_cx = function
  | CLiteral i -> string_of_int i
  | CBoollit true -> "True" | CBoollit false -> "False"
  | CFliteral s -> s
  | CTlit(ns, _size) -> "[" ^ String.concat ", " ns ^ "]"
  | CId s -> s
  | CUnop(Neg, e) -> "-" ^ string_of_cx (snd e) 
  | CAop(e1, op, e2) ->
    string_of_cx (snd e1) ^ " " ^ string_of_aop op ^ " " ^
    string_of_cx (snd e2)
  | CRop(e1, op, e2) ->
    string_of_cx (snd e1) ^ " " ^ string_of_rop op ^ " " ^
    string_of_cx (snd e2)
  | CBoolop(e1, op, e2) ->
    string_of_cx (snd e1) ^ " " ^ string_of_bop op ^ " " ^
    string_of_cx (snd e2)
  | CCondExpr(pred, cons, alt) -> 
    "if " ^ string_of_cx (snd pred) ^ " then " ^ string_of_cx (snd cons)
    ^ " else " ^ string_of_cx (snd alt)
  | CApp(f, args) -> string_of_cx (snd f) ^ "(" ^ 
    String.concat "," (List.map (fun a -> string_of_cx (snd a)) args)
  | CTensorIdx(i, e) -> string_of_cx (snd e) ^ "[" ^ string_of_int i ^ "]"

let product = List.fold_left ( * ) 1
let range n = 
    let rec range' n = match n with 0 -> [0] | _ -> n :: (range' (n-1)) 
    in range' (n - 1)  |> List.rev

let rec ctyp_of_styp = function
  | SNat -> CNat | SBool -> CBool | STensor [] -> CDouble
  | STensor shape -> CTensor(product shape, shape)
  | t -> Semant.last_stype t |> ctyp_of_styp


let rec offset shape indices = match List.tl shape, indices with
    [], [last_idx] -> last_idx
  | ns, i::is -> product ns * i + offset ns is
  | _ -> failwith "Invalid shapes and indices passed to offset"

(* Implement list monad functions *)
let return x = [x]
let (>>=) xs f = List.map f xs |> List.flatten
let rec sequence lst = match lst with
  | [] -> return []
  | x::xs -> x >>= fun v -> sequence xs >>= fun vs -> return (v::vs)


open Ast
open Sast

(* Between semant checking and codegen, run one more compiler pass to simplify
 * indexing *)
(* size and shape (shape is only for printing metadata) *)
type ctyp = CBool | CNat | CDouble | CTensor of int * int list

type cexpr = (ctyp * cx)

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
  | CTensorIdx of cexpr * int
  | CApp of cexpr * cexpr list

type assign =
  { name: string
  ; typ: ctyp
  ; index: int option
  ; (* None means this is a normal variable *)
  cexpr: cexpr }

type cfunc =
  { name: string
  ; ret_typ: ctyp
  ; params: (ctyp * string) list
  ; locals: assign list
  ; cexpr: cexpr }

let rec string_of_cx = function
  | CLiteral i -> string_of_int i
  | CBoollit true -> "True"
  | CBoollit false -> "False"
  | CFliteral s -> s
  | CTlit (ns, _size) -> "[" ^ String.concat ", " ns ^ "]"
  | CId s -> s
  | CUnop (Neg, e) -> "-" ^ string_of_cx (snd e)
  | CAop (e1, op, e2) ->
      string_of_cx (snd e1) ^ " " ^ string_of_aop op ^ " "
      ^ string_of_cx (snd e2)
  | CRop (e1, op, e2) ->
      string_of_cx (snd e1) ^ " " ^ string_of_rop op ^ " "
      ^ string_of_cx (snd e2)
  | CBoolop (e1, op, e2) ->
      string_of_cx (snd e1) ^ " " ^ string_of_bop op ^ " "
      ^ string_of_cx (snd e2)
  | CCondExpr (pred, cons, alt) ->
      "if " ^ string_of_cx (snd pred) ^ " then " ^ string_of_cx (snd cons)
      ^ " else " ^ string_of_cx (snd alt)
  | CApp (f, args) ->
      string_of_cx (snd f) ^ "("
      ^ String.concat "," (List.map (fun a -> string_of_cx (snd a)) args)
  | CTensorIdx (e, i) -> string_of_cx (snd e) ^ "[" ^ string_of_int i ^ "]"


let product = List.fold_left ( * ) 1

let range n =
  let rec range' n = match n with 0 -> [0] | _ -> n :: range' (n - 1) in
  range' (n - 1) |> List.rev


let rec ctyp_of_styp = function
  | SNat -> CNat
  | SBool -> CBool
  | STensor [] -> CDouble
  | STensor shape -> CTensor (product shape, shape)
  | t -> Semant.last_stype t |> ctyp_of_styp


let rec offset shape indices =
  match (List.tl shape, indices) with
  | [], [last_idx] -> last_idx
  | ns, i :: is -> product ns * i + offset ns is
  | _ -> failwith "Invalid shapes and indices passed to offset"


(* Implement list monad functions *)
let return x = [x]

let ( >>= ) xs f = List.map f xs |> List.flatten

let rec sequence lst =
  match lst with
  | [] -> return []
  | x :: xs -> x >>= fun v -> sequence xs >>= fun vs -> return (v :: vs)


let rec replace_indices sexpr indices =
  let ctyp = ctyp_of_styp @@ fst sexpr in
  ( ctyp
  , match snd sexpr with
    | SLiteral i -> CLiteral i
    | SBoolLit b -> CBoollit b
    | SFliteral s -> CFliteral s
    | STLit (contents, shape) -> CTlit (contents, product shape)
    | SId s -> (
      match List.assoc_opt s indices with
      | None -> CId s
      | Some i -> CLiteral i )
    | SUnop (Neg, e) -> CUnop (Neg, replace_indices e indices)
    | SAop (e1, op, e2) ->
        CAop (replace_indices e1 indices, op, replace_indices e2 indices)
    | SRop (e1, op, e2) ->
        CRop (replace_indices e1 indices, op, replace_indices e2 indices)
    | SBoolop (e1, op, e2) ->
        CBoolop (replace_indices e1 indices, op, replace_indices e2 indices)
    | SCondExpr (e1, e2, e3) ->
        CCondExpr
          ( replace_indices e1 indices
          , replace_indices e2 indices
          , replace_indices e3 indices )
    | STensorIdx (e, idxs) -> (
      match fst e with
      | STensor shape ->
          let i =
            offset shape (List.map (fun idx -> List.assoc idx indices) idxs)
          in
          CTensorIdx (replace_indices e indices, i)
      | _ -> raise (Failure "Fail to find tensor shape for tensor indexing") )
    | SApp (f, args) ->
        CApp
          ( replace_indices f indices
          , List.map (fun a -> replace_indices a indices) args )
    | Contract r -> (
        let contract_range = range (snd r.index) in
        match r.sexpr with
        | STensor [], STensorIdx ((STensor shape, tensor), idxs) ->
            let all_indices =
              List.map
                (fun i ->
                  match List.assoc_opt i indices with
                  | Some n -> [n]
                  | None ->
                      if i = fst r.index then contract_range
                      else failwith "Index not found in contract or in forall"
                  )
                idxs
              |> sequence
            in
            let summands =
              List.map
                (fun is ->
                  ( ctyp
                  , CTensorIdx
                      ( replace_indices (STensor shape, tensor) indices
                      , offset shape is ) ) )
                all_indices
            in
            List.fold_left
              (fun acc exp -> CAop ((CDouble, acc), Add, exp))
              (CFliteral "0") summands
        | _ -> failwith "Failure, semant failed (cast.ml line ~123)" )
    | Forall _ -> failwith "Should not call replace_indices on a forall" )

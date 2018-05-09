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
  { cname: string
  ; ret_typ: ctyp
  ; params: (ctyp * string) list
  ; locals: assign list
  ; cexpr: cexpr }

let string_of_ctyp = function
    CNat -> "CNat" | CBool -> "CBool" | CDouble -> "CDouble" 
  | CTensor(size, _) -> "CTensor[" ^ string_of_int size ^ "]"

let rec string_of_cexpr (_t, det) = match det with
  | CLiteral i -> string_of_int i
  | CBoollit true -> "True"
  | CBoollit false -> "False"
  | CFliteral s -> s
  | CTlit (ns, _size) -> "[" ^ String.concat ", " ns ^ "]"
  | CId s -> s
  | CUnop (Neg, e) -> "-" ^ string_of_cexpr e
  | CAop (e1, op, e2) ->
      string_of_cexpr e1 ^ " " ^ string_of_aop op ^ " "
      ^ string_of_cexpr e2
  | CRop (e1, op, e2) ->
      string_of_cexpr e1 ^ " " ^ string_of_rop op ^ " "
      ^ string_of_cexpr e2
  | CBoolop (e1, op, e2) ->
      string_of_cexpr e1 ^ " " ^ string_of_bop op ^ " "
      ^ string_of_cexpr e2
  | CCondExpr (pred, cons, alt) ->
      "if " ^ string_of_cexpr pred ^ " then " ^ string_of_cexpr cons
      ^ " else " ^ string_of_cexpr alt
  | CApp (f, args) ->
      string_of_cexpr f ^ "("
      ^ String.concat "," (List.map (fun a -> string_of_cexpr a) args) ^ ")"
  | CTensorIdx (e, i) -> 
      "{" ^ string_of_cexpr e ^ "[" ^ string_of_int i ^ "]" 
      ^ " : " ^ string_of_ctyp (fst e) ^ "}"

let string_of_assign r =
  let i_str =
    match r.index with None -> "" | Some i -> "[" ^ string_of_int i ^ "]"
  in
  r.name ^ i_str ^ " = " ^ string_of_cexpr r.cexpr 
  ^ " : " ^ string_of_ctyp r.typ ^ ";\n"


let string_of_cfunc r =
  let params_str = "(" ^ String.concat ", " (List.map snd r.params) ^ ")" in
  let locals_str =
    if r.locals = [] then ""
    else
      "\n{\n" ^ String.concat "\n" (List.map string_of_assign r.locals) ^ "}"
  in
  r.cname ^ params_str ^ " = " ^ string_of_cexpr r.cexpr ^ " : " ^
  string_of_ctyp r.ret_typ ^ locals_str

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
          in CTensorIdx (replace_indices e indices, i)
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
            let summands = List.map (fun is ->
                  ( CDouble , CTensorIdx
                      ( replace_indices (STensor shape, tensor) indices
                      , offset shape is ) ) )
                all_indices
            in
            List.fold_left
              (fun acc exp -> CAop ((CDouble, acc), Add, exp))
              (CFliteral "0") summands
        | _ -> failwith "Failure, semant failed (cast.ml line 179)" )
    | Forall _ -> failwith "Should not call replace_indices on a forall" )

let rec strip_indices cexpr = let t = fst cexpr in match snd cexpr with
  | CLiteral _ | CBoollit _ | CFliteral _ | CTlit _ | CId _ -> cexpr
  | CTensorIdx((CDouble, cexpr'), _) -> 
          strip_indices (CDouble, cexpr')
  | CTensorIdx(e, idxs) -> 
          t, CTensorIdx(strip_indices e, idxs) 
  | CUnop(Neg, e) -> t, CUnop(Neg, strip_indices e)
  | CAop(e1, op, e2) -> t, CAop(strip_indices e1, op, strip_indices e2)
  | CRop(e1, op, e2) -> t, CRop(strip_indices e1, op, strip_indices e2)
  | CBoolop(e1, op, e2) -> t, CBoolop(strip_indices e1, op, strip_indices e2)
  | CCondExpr(e1, e2, e3) -> t,
    CCondExpr(strip_indices e1, strip_indices e2, strip_indices e3)
  | CApp(f, args) -> t, CApp(strip_indices f, List.map strip_indices args)


let rec cexprs_of_sexpr sexpr =
  match snd sexpr with
  | Forall r ->
      let ranges = List.map (fun (_s, dim) -> range dim) r.indices in
      let all_indices = sequence ranges in
      let pairs =
        List.map (List.combine (List.map fst r.indices)) all_indices
      in
      List.map (fun pair -> replace_indices r.sexpr pair 
                |> strip_indices
        ) pairs
  | _ -> cexprs_of_sexpr (fst sexpr, Forall { indices= []; sexpr })


let assigns_of_sfunc sfunc =
  assert (sfunc.sfparams = []) ;
  if sfunc.lhs_indices = [] then
    [ { name= sfunc.sfname
      ; typ= ctyp_of_styp sfunc.stype
      ; index= None
      ; cexpr=
          (let cexpr =
             if cexprs_of_sexpr sfunc.sfexpr |> List.length = 1 then
               cexprs_of_sexpr sfunc.sfexpr |> List.hd
             else
               failwith
                 "Cannot have tensor quantity on rhs of non-indexed constant"
           in
           cexpr) } ]
  else
    let cexprs = cexprs_of_sexpr sfunc.sfexpr |> Array.of_list in
    assert (Array.length cexprs = product (List.map snd sfunc.lhs_indices)) ;
    Array.mapi
      (fun i cexpr -> {name= sfunc.sfname; typ= CDouble; index= Some i; cexpr})
      cexprs
    |> Array.to_list

type cprogram = cexpr * (assign list) * (cfunc list)

let cfunc_of_sfunc sfunc =
    let locals, cexpr = 
        if cexprs_of_sexpr sfunc.sfexpr |> List.length = 1 then 
           (sfunc.sscope >>= assigns_of_sfunc), 
           List.hd (cexprs_of_sexpr sfunc.sfexpr)     
        else
        let indices, shape = match sfunc.sfexpr with 
          | _, Forall r -> List.map fst (r.indices), List.map snd (r.indices)
          | _ -> failwith "unreachable (I think, please don't get here)" in
        let temp_sfunc = { sfname = sfunc.sfname ^ "~temp"
                         ; stype = STensor shape
                         ; sfparams = []
                         ; lhs_indices = List.combine indices shape
                         ; sfexpr = sfunc.sfexpr
                         ; sscope = [] } in 
        (sfunc.sscope >>= assigns_of_sfunc) @ assigns_of_sfunc temp_sfunc ,
        (CTensor(product shape, shape), CId(sfunc.sfname ^ "~temp"))
        in
    { cname = sfunc.sfname
    ; ret_typ = ctyp_of_styp sfunc.stype
    ; params = List.map (fun (t, str) -> ctyp_of_styp t, str) sfunc.sfparams
    ; locals = locals
    ; cexpr = cexpr
    }

let cprogram_of_sprogram (main, sfuncs) = 
    let svars, sfuncs = List.partition (fun sf -> sf.sfparams = []) sfuncs in
    List.hd (cexprs_of_sexpr main),
    svars >>= assigns_of_sfunc,
    List.map cfunc_of_sfunc sfuncs

let string_of_cprogram (main, assigns, cfuncs) = 
    "main = " ^ string_of_cexpr main ^ "\n" ^
    (((List.map string_of_assign) assigns) |> (String.concat "\n")) ^
    (((List.map string_of_cfunc) cfuncs) |> (String.concat "\n"))


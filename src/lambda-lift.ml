(* Given a Sast, perform lambda lifting on all functions in local scopes to make *)
(*     the job of codegen easier *)

open Sast
open Ast
module StringMap = Map.Make (String)

(* Find out which parameters in a function body are free variables *)
let get_free_vars sfunc = 
  let rec get_ids sexpr = match sexpr with
      _, SLiteral(_) | _, SBoolLit(_) | _, SFliteral(_) | _, STLit(_,_) -> []
    | _, SId(s) -> [s]
    | _, SUnop(Neg, e) -> get_ids e
    | _, SAop(e1, _, e2) -> get_ids e1 @ get_ids e2
    | _, SBoolop(e1, _, e2) -> get_ids e1 @ get_ids e2
    | _, SRop(e1, _, e2) -> get_ids e1 @ get_ids e2
    | _, SApp(fn, params) -> 
      get_ids fn @ List.flatten (List.map (fun e -> get_ids e) params)
    | _, SCondExpr(e1, e2, e3) -> 
      get_ids e1 @ get_ids e2 @ get_ids e3
    | _, STensorIdx(shape, tname, indices) -> 
      tname :: (Semant.ids_of_shape shape @ List.flatten 
                  (List.map get_ids indices)) 
  in () (*continue here *)


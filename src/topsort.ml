open Sast

(* Get a list of ids in the sexpr to be used in dependency analysis *)
let rec get_expr_ids id_list sexpr = match sexpr with
  | SLiteral(_) -> id_list
  | SFliteral(_) -> id_list
  | SBoolLit(_) -> id_list
  | STLit(_) -> id_list
  | SId(s) -> s::id_list
  | SUnop(_o, e) -> get_expr_ids id_list (snd e)
  | SAop((_t1, e1), _o, (_t2, e2)) -> 
    let id_list' = get_expr_ids id_list e1 in
        get_expr_ids id_list' e2
  | SBoolop((_t1, e1), _o, (_t2, e2)) -> 
    let id_list' = get_expr_ids id_list e1 in
        get_expr_ids id_list' e2
  | SRop((_t1, e1), _o, (_t2, e2)) -> 
    let id_list' = get_expr_ids id_list e1 in
        get_expr_ids id_list' e2
  | SApp((t, e), es) -> 
     List.fold_right (fun sx acc -> (get_expr_ids acc (snd sx))) es id_list
  | SCondExpr((t1, e1), (t2, e2), (t3, e3)) -> 
    let id_list' = get_expr_ids id_list e1 in
    let id_list'' = get_expr_ids id_list' e2 in
        get_expr_ids id_list'' e3
  | STensorIdx(_, _) -> raise (Failure "Not yet imlemented")

(*
let topsort sfuncs = 
  let zero_indegree = List.filter (fun sfunc: List.length sfunc.sfparams = 0)
      sfuncs
   *)

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
  | SApp((_t, _e), es) -> 
     List.fold_right (fun sx acc -> (get_expr_ids acc (snd sx))) es id_list
  | SCondExpr((_t1, e1), (_t2, e2), (_t3, e3)) -> 
    let id_list' = get_expr_ids id_list e1 in
    let id_list'' = get_expr_ids id_list' e2 in
        get_expr_ids id_list'' e3
  | STensorIdx(_, _) -> raise (Failure "Not yet imlemented")

type node = {
  data : sfunc;
  edges : string list;
}

(* Make record of sfunc with its corresponding incoming edges *)
let sfunc_to_node sfunc = 
  let unique_ids = List.sort_uniq compare (get_expr_ids [] (snd sfunc.sfexpr)) in
(*  let id_cmp id sfunc_list = List.exists (fun sfunc -> id = sfunc.sfname)
      sfunc_list in
  (* Filter funcs present in scope *)
  let scope_ids, fn_ids = List.partition 
      (fun id -> id_cmp id sfunc_list) unique_ids in
  (* Verify they're present in scope *)
  let scope_check ids = List.partition 
      (fun id -> id_cmp id sfunc.sscope) ids |> fst |> List.length in 
  match scope_check scope_ids with 
    0 ->*) { data = sfunc; edges = unique_ids }
  (*| _ -> raise (Failure "All ids in expr must be in global or fn scope")*)

let node_to_sfunc node = node.data

(* Remove id from explored list *)
let remove_id id node = 
 { data = node.data; 
   edges = List.filter (fun edge -> id <> edge) node.edges
 }

let topsort_elt sfunc nodes_list sorted_list = 
  let sorted_list' = sfunc::sorted_list in

  (* Returns if node contains an edge to this sfunc *)
  let contains_id id node = List.exists (fun edge -> id = edge) node.edges in

  (* Filter on sfuncs dependent on equivalent ids as this sfunc's name *)
  let sfunc_edges,others = List.partition (fun node -> 
      contains_id (sfunc.sfname) node) nodes_list in

  (* Update filtered nodes, removing edge of this variable dependency *)
  let updated_nodes = List.map (fun node -> 
      remove_id sfunc.sfname node) sfunc_edges in

  sorted_list', updated_nodes @ others


let rec topsort remaining_nodes sorted_list =
  (* Set of all nodes with no incoming edge *)
  let zero_indegree, others = List.partition (fun node -> 
      List.length node.edges = 0) remaining_nodes in
  begin
    match List.length zero_indegree with
      0 -> List.rev sorted_list 
    | _ -> let (sorted_list', remaining_nodes) = 
             topsort_elt (node_to_sfunc (List.hd zero_indegree)) 
               (List.tl zero_indegree @ others)
               sorted_list in
            topsort remaining_nodes sorted_list'
  end


let make_topsort (main_expr, sfuncs) = 
  let var_list, sfunc_list = List.partition (fun sfunc -> 
      (match sfunc.stype with
        SArrow(_,_) -> false
       | _ -> true)) sfuncs
  in
  let add_scope_vars var_list sfunc = List.fold_left 
      (fun acc sfunc -> sfunc::acc) var_list sfunc.sscope in
  let fold_list = var_list in
  let var_list' = List.fold_left 
      (fun acc sfunc -> add_scope_vars acc sfunc) fold_list var_list in 
  let nodes_list = List.map sfunc_to_node var_list' in
  let sorted_list = ((topsort nodes_list []) @ sfunc_list) in
  main_expr, sorted_list 

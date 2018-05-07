(* Given an Ast, return either a semantically checked Sast, or an error *)

open Ast
open Sast
module StringMap = Map.Make (String)

(* symbols map to lists of types because
 * this is how we represent the types * of functions *)
let base_map = StringMap.singleton "cast" (SArrow(SNat,STensor([])))
type symbol_table = styp StringMap.t

let rec styp_of_typ = function
  | Bool -> SBool
  | Nat -> SNat
  | Tensor s -> STensor (List.map (fun ax -> match ax with
             | ALiteral i -> i
             | _ -> failwith "internal error:
                    we shouldn't have any unresolved shapes here") s)
  | Arrow (t1, t2) -> SArrow (styp_of_typ t1, styp_of_typ t2)

let rec list_of_type = function
  | Bool -> [SBool] | Nat -> [SNat]
  | Tensor s -> [STensor (List.map (fun ax -> match ax with
               | ALiteral i -> i
               | _ -> failwith "internal error:
                      we shouldn't have any unresolved shapes here") s)]
  | Arrow (t1, t2) -> list_of_type t1 @ list_of_type t2
let but_last lst = List.rev @@ List.tl @@ List.rev lst
let last_type typ = list_of_type typ |> List.rev |> List.hd
let rec last_stype styp = match styp with
  | SArrow(_, t2) -> last_stype t2
  | t -> t


(* Create a table from a list of functions *)
let build_fns_table enclosing fns =
  let local_map = List.fold_left
    (fun table (ftyp, fdef) ->
    if StringMap.mem ftyp.ftyp_name table then
          failwith ("attempting to redefine already defined symbol: "
                    ^ fdef.fdef_name)
    else StringMap.add ftyp.ftyp_name (styp_of_typ ftyp.types) table)
        base_map fns in

  (* Combine local map with enclosing map,
   * throwing away the redundant variables in enclosing scope *)
  StringMap.union (fun _key _v1 v2 -> Some v2) enclosing local_map

(* Create a local table for a single function's arguments *)
let build_local_table enclosing (ftyp, fdef) =
  let types = but_last @@ list_of_type ftyp.types
  and params = fdef.fparams in

  let build_arg_map types params =
    List.fold_left2 (fun map typ param ->
    if StringMap.mem param map then failwith
     ("Non-linear pattern match encountered in definition of symbol " ^ param)
    else StringMap.add param typ map) base_map types params in

  let local_map = build_arg_map types params in
  StringMap.union (fun _key _v1 v2 -> Some v2) enclosing local_map

(* Pretty printing *)
let string_of_table map =
  (String.concat "\n" @@
  List.map (fun (name, types) -> name ^ " : " ^ (string_of_styp types)
           ) (StringMap.bindings map)) ^ "\n"

(* Type compare for Aops. Simple equality is not enough *)
let rec compare_styp t1 t2 =
    (* (1* Debug *1) *)
    (* print_endline @@ "Compared types " ^ string_of_styp t1 ^ " and " ^ *)
    (* string_of_styp t2; *)
    match t1 with
    | SNat | SBool -> t1 = t2
    (* make this more sophisticated later *)
    | STensor _ -> (match t2 with STensor _ -> true | _ -> false)
    | SArrow (t1', t2') ->
            match t2 with SArrow(t1'', t2'') ->
                  compare_styp t1' t1'' && compare_styp t2' t2''
                | _ -> false


(* Check expr: expr -> table -> indices -> sexpr * indices *)
let rec check_expr expression table indices =
    (* Finds the duplicated item in a sorted list *)
  let rec find_dup = function
      [] -> failwith "Internal error: called find_dup on indices with no dups"
    | [x] -> x
    | x :: y :: ys -> if x = y then x else find_dup (y :: ys) in
  let type_of expr = fst @@ fst (check_expr expr table indices) in
  (* Extract indices from expr *)
  let rec extract indices expr =
      let combine map1 map2 = StringMap.union (fun _ n1 n2 ->
            if n1 = n2 then Some n2 else failwith @@ "Error - tried to rebind
            indices in " ^ string_of_expr expr) map1 map2 in
      match expr with
    | Literal _ | Fliteral _ | BoolLit _ | TLit _ | TFile _ | Id _ -> indices
    | TensorIdx(e, ixs) -> (match type_of e with
      | STensor(shape) -> List.fold_left2 (fun acc idx num ->
              match StringMap.find_opt idx acc with
                | None -> StringMap.add idx num acc
                | Some num' -> if num' = num then acc else
                failwith @@ "Cannot rebind index " ^ idx
                ^ ". It was already bound to " ^ string_of_int num'
                ^ " and you are trying to rebind it to " ^ string_of_int num
              ) indices ixs shape
      | t -> failwith @@
             "Type error: cannot index expression of type " ^ string_of_styp t
    )
    | Unop(_, e) -> extract indices e
    | Aop(e1, _, e2) | Boolop(e1, _, e2) | Rop(e1, _, e2) | App(e1, e2)
        -> combine (extract indices e1) (extract indices e2)
    | CondExpr(e1, e2, e3) -> combine (extract indices e3) @@
        combine (extract indices e1) (extract indices e2)
  in
  let indices = extract indices expression in

  (* Lookup symbols in index table first because they should shadow *)
  let rec lookup_symb symb =
    match StringMap.find_opt symb indices with
      | Some _num -> SNat
      | None -> match StringMap.find_opt symb table with
        | Some typ -> typ
        | None -> failwith ("Encountered undefined symbol: " ^ symb) in

  match (expression : expr) with
    | Literal(i) -> (SNat , SLiteral(i)), indices
    | Fliteral(s) -> (STensor([]), SFliteral(s)), indices
    | BoolLit(b) -> (SBool, SBoolLit(b)), indices
    | TLit(_) ->
            let t = Infer.verify expression in if t then
            let shape = Infer.build_shape expression in
            let components = Infer.flatten expression in
            let unwrap_components = List.map (fun c -> match c with
             | Fliteral(f) -> f
             | _ -> failwith "Internal error: non-float encounted in
            tensor literal"
            ) in
            (STensor(shape), STLit(unwrap_components components, shape)), indices
            else failwith "Invalid tensor literal"
    | TFile(filepath) ->
     (* line-reader from
      * http://camltastic.blogspot.com/2008/09/
      * tip-read-all-lines-from-file-most.html
      * *)
            let readfile fpath = 
              let lines = ref [] in
              let channel = open_in fpath in
              try
                while true; do
                  lines := input_line channel :: !lines
                done; []
              with End_of_file ->
                close_in channel;
                List.rev !lines in
            let tensor_string = readfile filepath in
            let shape = List.filter
                (fun substring -> String.length substring > 0)
                (String.split_on_char ' ' (List.hd tensor_string)) in
            let shape' = List.map int_of_string shape in
            let components = (String.split_on_char ' ' (String.concat " "
                                                (List.tl tensor_string))) in
            (STensor(shape'), STLit((components), shape')), indices
    | Id(s) -> (lookup_symb s, SId(s)), indices
    | Unop(Neg, expr) -> begin
        match type_of expr with
          STensor [] ->
            (STensor [], SUnop(Neg, fst @@ check_expr expr table indices)), indices
        | _ -> failwith @@
        "Type error: cannot negate something that isn't an indexed tensor.
        Error in expression: " ^ string_of_expr expr
    end
    | Aop(expr1, op, expr2) ->
        if not @@ compare_styp (type_of expr1) (type_of expr2) then
        failwith "Detected arithmetic operation on incompatible types" else
        begin
            match type_of expr1, type_of expr2 with
            | SNat, SNat -> (SNat,
                    SAop((fst @@ check_expr expr1 table indices),
                        op, (fst @@ check_expr expr2 table indices))), indices
            | SBool, _ -> failwith "Detected arithmetic operation on boolean"
            | STensor([]), STensor([]) -> (STensor([]),
                    SAop((fst @@ check_expr expr1 table indices),
                        op, (fst @@ check_expr expr2 table indices))), indices
            | STensor _, STensor _ ->
                let sexpr1, _ = check_expr expr1 table indices in
                let sexpr2, _ = check_expr expr2 table indices in
            begin
                match op with
                | Add | Sub | Mod | Expt | Div ->
                    begin
                      match sexpr1, sexpr2 with
                      | (STensor shape1, STensorIdx(_, idxs1)),
                        (STensor shape2, STensorIdx(_, idxs2)) ->
                        if not @@ (shape1 = shape2) || (shape1 = []) || (shape2 = [])
                        then failwith "Type error, cannot perform
                        non-multiplicative operations on tensors of different
                        shapes unless one of them is rank 0."
                        else if shape1 = shape2 && idxs1 <> idxs2 then
                            failwith "If the two tensors are indeed of the same
                            shape, then you need to index them accordingly."
                        else
            let ret_type = if shape1 = [] then STensor shape2 else STensor shape1 in
            let ret_idxs = if idxs1 = [] then idxs2 else idxs1 in
            let ret_expr = ret_type, SAop(sexpr1, op, sexpr2)
            in (ret_type, STensorIdx(ret_expr, ret_idxs)), indices
                      | _ -> failwith "Type error line 175"
                    end

                | Mult ->
                    begin
                      match sexpr1, sexpr2 with
                      | (STensor shape1, STensorIdx(_, idxs1)),
                        (STensor shape2, STensorIdx(_, idxs2)) ->
            let all_idxs = idxs1 @ idxs2 in
            if List.sort compare all_idxs = List.sort_uniq compare all_idxs
            then
            let new_shape = shape1 @ shape2 in
            let new_idxs = idxs1 @ idxs2 in
            (STensor new_shape,
                STensorIdx((STensor new_shape, SAop(sexpr1, Mult, sexpr2)),
                            new_idxs)), indices
            else if List.length (List.sort compare all_idxs) =
                    List.length (List.sort_uniq compare all_idxs) + 1 then
            let the_index = find_dup (List.sort compare all_idxs) in
            let new_idxs, new_shape =
                List.filter (fun (i, _) -> i <> the_index) (List.combine all_idxs
                (shape1 @ shape2)) |> List.split in
            let the_size =
                List.filter (fun (i, _) -> i = the_index)
                    (List.combine all_idxs (shape1 @ shape2))
                |> List.hd |> snd in
            let the_sexpr = (STensor new_shape,
                Contract { index = the_index, the_size;
                sexpr = STensor (shape1 @ shape2),
                        SAop(sexpr1, Mult, sexpr2) }
                )
            in (STensor new_shape, STensorIdx(the_sexpr, new_idxs)),
            StringMap.remove the_index indices
            else failwith "Type error line 216"


                      | _ -> failwith "Type error in tensor multiplication"
                    end


            end
            | SArrow(_,_), _ ->
                failwith "Arithmetic operation on partially applied function"
            | _ -> failwith @@ "Type error in " ^ string_of_expr expression
        end
    | Boolop(expr1, op, expr2) -> if type_of expr1 <> type_of expr2 then
        failwith "Detected boolean operation on incompatible types" else
        begin
            match type_of expr1 with
            | SNat -> failwith "Detected boolean operation on Nats"
            | SBool -> (SBool,
                    SBoolop((fst @@ check_expr expr1 table indices),
                        op, (fst @@ check_expr expr2 table indices))), indices
            | STensor(_) ->
                    failwith "Detected boolean operation on incompatible types"
            | SArrow(_,_) ->
                    failwith "Boolean operation on partially applied function"
        end
    | Rop(expr1, op, expr2) -> if type_of expr1 <> type_of expr2 then
        failwith "Detected relational operation on incompatible types" else
        begin
            match type_of expr1 with
            | SNat | STensor [] -> (SBool,
                    SRop((fst @@ check_expr expr1 table indices),
                        op, (fst @@ check_expr expr2 table indices))), indices
            | SBool -> failwith "Detected relational operation on boolean"
            | STensor(_) -> failwith "No ordering exists on arbitrary rank tensors"
            | SArrow(_,_) ->
                    failwith "Relational operation on partially applied function"
        end
    | App(expr1, expr2) ->
      begin
        match expr1 with
        | App(expr1', expr2') ->
          begin
            match type_of expr1' with
            | SArrow(first_param_type, remaining_types) ->
              if type_of expr2' = first_param_type
                 then (last_stype remaining_types,
                 SApp(fst @@ check_expr expr1' table indices,
                      fst (check_expr expr2' table indices)
                      :: [fst @@ check_expr expr2 table indices])), indices
              else
                  failwith ("Expected type " ^ string_of_styp first_param_type
                     ^ " but instead received " ^ string_of_styp (type_of expr2))
            | _ -> failwith "Type error"
          end
        | _ ->
          begin
            match type_of expr1 with
            | SArrow(param_type, return_type) ->
              if type_of expr2 = param_type
                 then (return_type, SApp(fst @@ check_expr expr1 table indices,
                 [fst @@ check_expr expr2 table indices])), indices
              else
                  failwith ("Expected type " ^ string_of_styp param_type
                     ^ " but instead received " ^ string_of_styp (type_of expr2))
            | _ -> failwith "Type error"
          end
      end
    | CondExpr(expr1, expr2, expr3) -> if type_of expr1 <> SBool
        then failwith "Non-boolean expression in if statement"
        else if type_of expr2 <> type_of expr3 then
        failwith "Incompatible types in conditional expressions" else
        (type_of expr2,
         SCondExpr(fst @@ check_expr expr1 table indices,
                   fst @@ check_expr expr2 table indices,
                   fst @@check_expr expr3 table indices)), indices
    | TensorIdx(e, idxs) ->
        if List.sort compare idxs = List.sort_uniq compare idxs then
        (type_of e, STensorIdx(fst @@ check_expr e table indices, idxs)), indices
        else if List.length (List.sort compare idxs) =
                List.length (List.sort_uniq compare idxs) + 1 then
        let the_index = find_dup (List.sort compare idxs) in
        let old_shape = match type_of e with
          | STensor old_shape -> old_shape
          | _ -> failwith "Type error - cannot index non-tensor object" in
        let new_idxs, new_shape =
          List.filter (fun (i, _) -> i <> the_index) (List.combine idxs old_shape)
            |> List.split in
        let the_size =
          List.filter (fun (i, _) -> i = the_index) (List.combine idxs old_shape)
            |> List.hd |> snd in
        let the_sexpr = (STensor new_shape, Contract { index = the_index, the_size;
                sexpr = STensor old_shape,
                        STensorIdx(fst @@ check_expr e table indices, idxs) }) in
        (STensor new_shape, STensorIdx(the_sexpr, new_idxs)),
            StringMap.remove the_index indices
        else failwith @@ "Cannot contract more than one set of indices in a single
                            tensor. Failed at " ^ string_of_expr e

(* If a function has a single type in its decl and the same
 * id appears in its definition raise error, else return true*)
let recursive_check (ftyp, fdef) =
  let ftyp' = ftyp.types in
  let single_typ = match ftyp' with
    | Nat | Bool | Tensor(_) -> true
    | Arrow(_) -> false in
  let fid = ftyp.ftyp_name in
  let rec rec_def fexpr = match fexpr with
    | Literal(_) | Fliteral(_) | BoolLit(_) | TLit(_) | TFile(_) -> false
    | Unop(_, e) -> rec_def e
    | Aop(e1, _, e2) -> rec_def e1 && rec_def e2
    | Boolop(e1, _, e2) -> rec_def e1 && rec_def e2
    | Rop(e1, _, e2) -> rec_def e1 && rec_def e2
    | App(e1, e2) -> rec_def e1 && rec_def e2
    | CondExpr(e1, e2, e3) -> rec_def e1 && rec_def e2 && rec_def e3
    (* For now, but I doubt this could actually be a problem*)
    | TensorIdx(_, _e) -> false
    | Id(s) -> if s = fid then
        failwith "Recursively defined Nat/Bool/Tensor not permitted"
        else false in
  let main_expr = fdef.main_expr in
  if single_typ && rec_def main_expr then false else true

(* The is_scalar function checks if a tensor is indexed enough to be able to
 * be on the RHS of something with an indexed LHS. *)
let rec is_scalar sexpr = match fst sexpr with
  | STensor shape -> begin
          match snd sexpr with
      | SLiteral _ | SBoolLit _ | SFliteral _ -> true
      | STLit _ -> false
      | SId _ -> shape = []
      | SUnop(_, e) -> is_scalar e
      | SBoolop _ | SRop _ ->
              failwith "internal error, semant failed. Shouldn't have Boolops
              or Rops in consideration for being scalars"
      | SCondExpr(_, e2, e3) -> is_scalar e2 && is_scalar e3
      | STensorIdx _ -> true
      | SAop(e1, _, e2) -> is_scalar e1 && is_scalar e2
      | SApp(_e, _es) -> shape = []
      | Contract _ -> shape = []
      | Forall _ -> failwith "Forall shouldn't be introduced yet"
    end
  | _ -> false

(* Check a single function - return sfunc or error *)
let rec check_func enclosing (ftyp, fdef) =
    let lhs_indices = match String.contains fdef.fdef_name '[' with
      | false -> StringMap.empty
      | true ->
          let indices = List.nth (String.split_on_char '[' fdef.fdef_name) 1 in
          let indices = String.sub indices 0 (String.length indices - 1) in
          let indices = String.split_on_char ',' indices in
          match ftyp.types with
            | Tensor(shape) -> List.fold_left2 (fun acc idx num ->
                match num with
                  | ALiteral n -> (match StringMap.find_opt idx acc with
                      | None -> StringMap.add idx n acc
                      | Some n' -> if n' = n then acc else
                        failwith @@ "Cannot rebind index " ^ idx
                        ^ ". It was already bound to " ^ string_of_int n'
                        ^ " and you are trying to rebind it to " ^ string_of_int n
                      )
                  | _ -> failwith "internal error: infer failed"
                ) StringMap.empty indices shape
            | t -> failwith @@ "Type error: " ^ "cannot index entity of type " ^
                   string_of_typ t in

  let table' = build_local_table enclosing (ftyp, fdef) in
  let table  = build_fns_table table' fdef.scope in
  let this_sexpr, rhs_indices = check_expr fdef.main_expr table lhs_indices in
  let all_indices =
      StringMap.union (fun _ _ v -> Some v) rhs_indices lhs_indices in
  let sfparams = List.fold_right2 (fun typ arg acc -> (typ, arg)::acc)
    (list_of_type ftyp.types |> but_last) fdef.fparams [] in

  if
       StringMap.is_empty all_indices &&
       recursive_check (ftyp, fdef) &&
       last_type ftyp.types = fst this_sexpr
  then
    { sfname = ftyp.ftyp_name; stype = styp_of_typ ftyp.types;
      slocals = []; (* for now *)
      sfparams = sfparams;
      sfexpr = this_sexpr;
      sscope = List.map (check_func table) fdef.scope }
  else if
       StringMap.is_empty lhs_indices &&
       recursive_check (ftyp, fdef) &&
       last_type ftyp.types = fst this_sexpr
  then
      let this_sexpr' = (fst this_sexpr,
      Forall { indices = StringMap.bindings rhs_indices; sexpr = this_sexpr; }) in
    { sfname = ftyp.ftyp_name; stype = styp_of_typ ftyp.types;
      slocals = []; (* for now *)
      sfparams = sfparams;
      sfexpr = this_sexpr';
      sscope = List.map (check_func table) fdef.scope }
  else
       let module Set = Set.Make (String) in
       let rhs_set = StringMap.fold (fun s _ acc -> Set.add s acc)
                     rhs_indices Set.empty in
       let lhs_set = StringMap.fold (fun s _ acc -> Set.add s acc)
                     lhs_indices Set.empty in
       if
       not (StringMap.is_empty lhs_indices) &&
       Set.subset rhs_set lhs_set &&
       recursive_check (ftyp, fdef) &&
       is_scalar this_sexpr &&
       match ftyp.types with Tensor _ -> true | _ -> false
  then
      let this_sexpr' = (fst this_sexpr,
      Forall { indices = StringMap.bindings all_indices; sexpr = this_sexpr; }) in
    { sfname = ftyp.ftyp_name; stype = styp_of_typ ftyp.types;
      slocals = []; (* for now *)
      sfparams = sfparams;
      sfexpr = this_sexpr';
      sscope = List.map (check_func table) fdef.scope }

  else failwith @@ "Declared type " ^ string_of_typ ftyp.types
           ^ " but received type " ^ string_of_styp @@ fst this_sexpr

(* Check entire program *)
let check (main_expr, funcs) =
  (* Infer all tensor shapes *)
  let funcs = Infer.deduce_shapes funcs in
  (* Build table of functions in global scope *)
  let global_table = build_fns_table StringMap.empty funcs in
  let check_main, _ = check_expr main_expr global_table StringMap.empty in
  match check_main with
    | SBool, _ | SNat, _ | STensor(_), _ ->
            (check_main, List.map (check_func global_table) funcs)
    | _ -> failwith "main must be of type Tensor, Nat, or Bool"

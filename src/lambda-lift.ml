(* Given a Sast, perform lambda lifting on all functions in local scopes to make *)
(*     the job of codegen easier *)

open Sast
open Ast
module StringMap = Map.Make (String)

(* Utilities for getting ids out of shapes and types *)
let ids_of_shape shape =
  let rec ids_of_aexpr = function
    | ALiteral(_) -> []
    | AId(s) -> [s]
    | AAop(e1, _, e2) -> ids_of_aexpr e1 @ ids_of_aexpr e2
    | AApp(e1, e2) -> ids_of_aexpr e1 @ ids_of_aexpr e2 in
  List.flatten @@ List.map (fun aexpr -> ids_of_aexpr aexpr) shape

let rec ids_of_type = function
  | Unit(Bool) | Unit(Nat) -> []
  | Unit(Tensor(shape)) -> ids_of_shape shape
  | Arrow(t1, t2) -> ids_of_type t1 @ ids_of_type t2

(* Create a table from a list of functions *)
let build_fns_table enclosing fns =
  let local_map' = List.fold_left
    (fun table sfunc ->
    if StringMap.mem sfunc.sfname table then raise
          (Failure ("attempting to redefine already defined symbol: "
                    ^ sfunc.sfname))
    else StringMap.add sfunc.sfname sfunc.stype table) StringMap.empty fns in

  (* Now add tensor shapes to the local_map *)
  let unique_shape_vars = List.sort_uniq compare @@ List.flatten @@
    List.map (fun sfunc -> ids_of_type @@ sfunc.stype) fns in

  let local_map = List.fold_left
    (fun table shape_var ->
      if StringMap.mem shape_var table then raise
          (Failure ("attempting to redefine already defined symbol: "
                    ^ shape_var))
      else StringMap.add shape_var (Unit(Nat)) table) local_map' unique_shape_vars
  in
  (* Combine local map with enclosing map,
   * throwing away the redundant variables in enclosing scope *)
  StringMap.union (fun _key _v1 v2 -> Some v2) enclosing local_map


(* Create a local table for a single function's arguments *)
let build_local_table enclosing sfunc =
  (* define local utilities *)
  let rec list_of_type typ = match typ with
    | Unit(t) -> [t] | Arrow(t1, t2) -> list_of_type t1 @ list_of_type t2 in
  let but_last lst = List.rev @@ List.tl @@ List.rev lst in
  let types = but_last @@
    list_of_type sfunc.stype and params = sfunc.sfparams in
  let build_arg_map types params =
    List.fold_left2 (fun map typ param ->
    if StringMap.mem param map then raise (Failure
     ("Non-linear pattern match encountered in definition of symbol " ^ param))
    else StringMap.add param (Unit(typ)) map) StringMap.empty types params in

  let local_map = build_arg_map types params in
  (* Combine local map with enclosing map,
   * throwing away the redundant variables in enclosing scope *)
  StringMap.union (fun _key _v1 v2 -> Some v2) enclosing local_map

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

(* For each func found this needs to give it unique name and then
  * update all its call sites in its enclosing scope and sscope *)
let uname_sfunc_table = StringMap.empty
let uname_it name = name
(* StringMap -> sfunc list -> sfunc list *)
let rec unique_name enclosing sfunc =
  let local_scope = build_local_table enclosing sfunc in
  { sfname = uname_it sfunc.sfname; stype = sfunc.stype;
    sfparams = sfunc.sfparams; sfexpr = sfunc.sfexpr;
    sscope = List.map (fun f -> unique_name local_scope f) sfunc.sscope }


let lamda_lift (main_sexpr, sfuncs) =
  let global_table = build_fns_table StringMap.empty sfuncs in
  let uniquely_named_sfuncs = List.map (fun f -> unique_name global_table f)
  sfuncs in (main_sexpr, uniquely_named_sfuncs)

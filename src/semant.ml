(* Given an Ast, return either a semantically checked Sast, or an error *)

open Sast
open Ast
module StringMap = Map.Make (String)

type symbol_table = {
  (* symbols map to lists of types because this is how we represent the types
   * of functions *) 
  cur_scope : (typ list) StringMap.t;
  parent : symbol_table option;
}

let empty_table = { cur_scope = StringMap.empty; parent = None }
let new_child_table tbl = { cur_scope = StringMap.empty; parent = Some tbl}

(* Create a table from a list of functions
 * Currently only implementing Nats and Bools *)
let build_global_table =
    let rec build_table tbl  = function
      | ((ftyp, fdef) :: fns) ->
      let symbol = fdef.fdef_name in
      let types = ftyp.types in
      if StringMap.mem symbol tbl.cur_scope then raise 
          (Failure ("attempting to redefine already defined symbol" ^ symbol))
      else 
        build_table { cur_scope = StringMap.add symbol types tbl.cur_scope;
                      parent = tbl.parent;
                    } fns
      | [] -> tbl
in build_table empty_table

let build_arg_table (ftyp, fdef) parent = 
 let param_types = List.tl ftyp.types in
 let assoc_list = List.map2 (fun param typ -> (param, [typ])) 
                    fdef.fparams param_types in
 let cur_scope = List.fold_left (fun table (param, typ) -> 
        if StringMap.mem param table then raise
            (Failure ("\nUse of non-linear pattern " ^ param ^ "\n"))
        else StringMap.add param typ table) StringMap.empty assoc_list
in { cur_scope = cur_scope; parent = parent }

(* Pretty printing *)
let string_of_table { cur_scope = m; parent = p} =
  let string_of_table' map =  
  (String.concat "\n" @@
  List.map (fun (name, types) -> name ^ " : " ^ String.concat " -> " 
                (List.map string_of_typ types)
           ) (StringMap.bindings map)) ^ "\n"
  in match p with
    | None -> string_of_table' m
    | Some parent -> string_of_table' parent.cur_scope
                     ^ "{\n" ^ string_of_table' m ^ "\n}\n"

(* Check functions: return sfunc list or error *)
(* let rec check_expr expression table = (* WIP *) *)

let rec check_funcs funcs = 
  let global_table = build_global_table in
  match funcs with
    | [] -> []
    | (ftyp, fdef) :: fns ->
    let local_table = build_arg_table (ftyp, fdef) Some global_table in
    let this_sexpr = check_expr fdef.main_expr local_table in
    let this_func = { sfname = ftyp.ftyp_name; stype = ftyp.types; 
                      sfparams = fdef.fparams; sfexpr = this_sexpr; sscope = [] }
  in this_func :: check_funcs fn

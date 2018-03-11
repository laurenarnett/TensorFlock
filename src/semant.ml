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

let string_of_table { cur_scope = m; parent = p} = 
  String.concat "\n" @@
  List.map (fun (name, types) -> name ^ " : " ^ String.concat " -> " 
                (List.map string_of_typ types)
           ) (StringMap.bindings m)
    


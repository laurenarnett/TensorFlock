(* Given an Ast, return either a semantically checked Sast, or an error *)

open Sast
open Ast
module StringMap = Map.Make (String)

type symbol_table = {
  (* symbols map to lists of types because this is how we represent the types
   * of functions *) 
  cur_scope : typ StringMap.t;
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
  let rec list_of_type typ = match typ with 
    | Unit(t) -> [t] | Arrow(t1, t2) -> list_of_type t1 @ list_of_type t2 in
  let but_last lst = List.rev lst |> List.tl |> List.rev in
  let types = but_last @@ list_of_type ftyp.types and params = fdef.fparams in
  let build_arg_map types params = List.fold_left2 (fun map typ param ->
    if StringMap.mem param map then raise (Failure 
     ("Non-linear pattern match encountered in definition of symbol " ^ param))
    else StringMap.add param (Unit(typ)) map) StringMap.empty types params
in { cur_scope = build_arg_map types params; parent = parent }


(* Pretty printing *)
let string_of_table { cur_scope = m; parent = p} =
  let string_of_table' map =  
  (String.concat "\n" @@
  List.map (fun (name, types) -> name ^ " : " ^ (string_of_typ types)
           ) (StringMap.bindings map)) ^ "\n"
  in match p with
    | None -> string_of_table' m
    | Some parent -> string_of_table' parent.cur_scope
                     ^ "{\n" ^ string_of_table' m ^ "\n}\n"

let find' key map = 
    try Some (StringMap.find key map)
    with Not_found -> None

let rec lookup_symb symb { cur_scope = m; parent = p} = 
  match find' symb m with
    | None -> (match p with
                | None -> raise (Failure "Undefined symbol")
                | Some table -> lookup_symb symb table)
    | Some typ_list -> typ_list
    
    
(* Check functions: return sfunc list or error *)
let rec check_expr expression table = 
  match expression with
    | Literal(i) -> (Unit(Nat), SLiteral(i))
    | Fliteral(s) -> (Unit(Tensor([])), SFliteral(s))
    | BoolLit(b) -> (Unit(Bool), SBoolLit(b))
    | TLit(_) -> raise (Failure "not yet implemented")
    | Id(s) -> (lookup_symb s table, SId(s))


let rec check_funcs funcs = 
  let global_table = build_global_table funcs in
  match funcs with
    | [] -> []
    | (ftyp, fdef) :: fns ->
    let local_table = build_arg_table (ftyp, fdef) (Some global_table) in
    let this_sexpr = check_expr fdef.main_expr local_table in
    let this_func = { sfname = ftyp.ftyp_name; stype = ftyp.types; 
                      sfparams = fdef.fparams; sfexpr = this_sexpr; sscope = [] }
  in this_func :: check_funcs fns

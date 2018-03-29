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
  let but_last lst = List.rev @@ List.tl @@ List.rev lst in
  let types =
    but_last @@
    list_of_type ftyp.types and params = fdef.fparams in
  let build_arg_map types params =
    List.fold_left2 (fun map typ param ->
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

(* Tensor literal checking functions *)
let rec flatten expr = match expr with (TLit(l)) -> (match l with
  | Fliteral(_) :: _ -> l
  | _ -> List.flatten (List.map flatten l))
  | _ -> raise (Failure "can't flatten a non_tlit expr")

let rec build_shape expr = match expr with
  | Fliteral(_) -> []
  | TLit(l) -> List.length l :: (build_shape (List.hd l))
  | _ -> raise (Failure "Internal error: cannot call build_shape on non-tensor-literal expression")

let rec verify expr = match expr with (TLit(l)) -> (match List.hd l with
  | Fliteral(_) -> true
  | TLit(x) -> List.for_all (fun c -> (match c with
        | TLit(component) -> List.length component = List.length x
        | _ -> raise (Failure "Invalid entity in tensor literal"))) l
        && List.for_all (fun x -> x) (List.map verify l)
  | _ -> raise (Failure "Invalid entity in tensor literal"))
  | _ -> raise (Failure "internal error: can't verify non_tensor_literal")

(* Check functions: return sfunc list or error *)
let rec check_expr expression table =
  let type_of expr = fst (check_expr expr table) in
  match (expression : expr) with
    | Literal(i) -> (Unit(Nat), SLiteral(i))
    | Fliteral(s) -> (Unit(Tensor([])), SFliteral(s))
    | BoolLit(b) -> (Unit(Bool), SBoolLit(b))
    | TLit(_) -> let t = verify expression in if t then
                 let shape = build_shape expression in
                 let components = flatten expression in
                 let unwrap_components = List.map (fun c -> match c with
                  | Fliteral(f) -> f
                  | _ -> raise (Failure "Internal error: non-float encounted in
                 tensor literal")
                 ) in
                 (Unit(Tensor(List.map (fun s -> ALiteral(s)) shape)),
                 STLit(unwrap_components components, shape))
                 else raise (Failure "Invalid tensor literal")
    | Id(s) -> (lookup_symb s table, SId(s))
    | Unop(Neg, _expr) -> raise (Failure
          "You can't negate Nats and tensors haven't been implemented yet")
    | Aop(expr1, op, expr2) -> if type_of expr1 <> type_of expr2 then raise
        (Failure "Detected arithmetic operation on incompatible types") else
        begin
            match type_of expr1 with
                | Unit(Nat) -> (Unit(Nat),
                        SAop((check_expr expr1 table), op, (check_expr expr2 table)))
                | Unit(Bool) -> raise (Failure "Detected arithmetic operation on boolean")
                | Unit(Tensor(_)) -> raise (Failure "Not yet implemented")
                | Arrow(_,_) -> raise
                        (Failure "Arithmetic operation on partially applied function")
        end
    | Boolop(expr1, op, expr2) -> if type_of expr1 <> type_of expr2 then raise
        (Failure "Detected boolean operation on incompatible types") else
        begin
            match type_of expr1 with
            | Unit(Nat) -> raise (Failure "Detected boolean operation on Nats")
            | Unit(Bool) -> (Unit(Bool),
                    SBoolop((check_expr expr1 table), op, (check_expr expr2 table)))
            | Unit(Tensor(_)) -> raise
                    (Failure "Detected boolean operation on incompatible types")
            | Arrow(_,_) -> raise
                    (Failure "Boolean operation on partially applied function")
        end
    | Rop(expr1, op, expr2) -> if type_of expr1 <> type_of expr2 then raise
        (Failure "Detected relational operation on incompatible types") else
        begin
            match type_of expr1 with
            | Unit(Nat) -> (Unit(Bool),
                    SRop((check_expr expr1 table), op, (check_expr expr2 table)))
            | Unit(Bool) -> raise (Failure "Detected relational operation on boolean")
            | Unit(Tensor(_)) -> raise (Failure "Not yet implemented")
            | Arrow(_,_) -> raise
                        (Failure "Relational operation on partially applied function")
        end
    | App(expr1, expr2) ->
      begin
        match expr1 with
        | App(expr1', expr2') ->
          begin
            match type_of expr1' with
            | Arrow(first_param_type, remaining_types) ->
              let rec last_type types =
                  (match types with | Arrow(_, ts) -> last_type ts
                                    | Unit(t) -> Unit(t)) in
              if type_of expr2' = first_param_type
                 then (last_type remaining_types, SApp(check_expr expr1' table,
                                                 check_expr expr2' table ::
                                                 [check_expr expr2 table]))
              else
                 raise (Failure ("Expected type " ^ string_of_typ first_param_type
                       ^ " but instead received " ^ string_of_typ (type_of expr2)))
            | _ -> raise (Failure "Type error")
          end
        | _ ->
          begin
            match type_of expr1 with
            | Arrow(param_type, return_type) ->
              if type_of expr2 = param_type
                 then (return_type, SApp(check_expr expr1 table, [check_expr expr2 table]))
              else
                 raise (Failure ("Expected type " ^ string_of_typ param_type
                       ^ " but instead received " ^ string_of_typ (type_of expr2)))
            | _ -> raise (Failure "Type error")
          end
      end
    | CondExpr(expr1, expr2, expr3) -> if type_of expr1 <> Unit(Bool)
        then raise (Failure "Non-boolean expression in if statement")
        else if type_of expr2 <> type_of expr3 then raise
        (Failure "Incompatible types in conditional expressions") else
        (type_of expr2,
         SCondExpr(check_expr expr1 table,
                   check_expr expr2 table, check_expr expr3 table))
    | TensorIdx(_,_) -> raise (Failure "Not yet implemented")


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

(* Check entire program *)
let check (main_expr, funcs) =
  let global_table = build_global_table funcs in
  let check_main = check_expr main_expr global_table in
  match check_main with
    | (Unit(_), _) -> (check_main, check_funcs funcs)
    | _ -> raise (Failure "main must be of type Tensor, Nat, or Bool")

(* Given an Ast, return either a semantically checked Sast, or an error *)

open Sast
open Ast
module StringMap = Map.Make (String)

(* symbols map to lists of types because
 * this is how we represent the types * of functions *)
type symbol_table = typ StringMap.t

(* Create a table from a list of functions *)
let build_fns_table enclosing fns =
  let local_map' = List.fold_left
    (fun table (ftyp, fdef) ->
    if StringMap.mem fdef.fdef_name table then raise
          (Failure ("attempting to redefine already defined symbol: "
                    ^ fdef.fdef_name))
    else StringMap.add fdef.fdef_name ftyp.types table) StringMap.empty fns in

  (* Now add tensor shapes to the local_map *)
  let rec ids_of_aexpr = function
    | ALiteral(_) -> []
    | AId(s) -> [s]
    | AAop(e1, _, e2) -> ids_of_aexpr e1 @ ids_of_aexpr e2
    | AApp(e1, e2) -> ids_of_aexpr e1 @ ids_of_aexpr e2 in
  let ids_of_shape shape =
    List.flatten @@
    List.map (fun aexpr -> ids_of_aexpr aexpr) shape in
  let rec ids_of_type = function
      Bool | Nat -> []
    | Tensor(shape) -> ids_of_shape shape
    | Arrow(t1, t2) ->
      ids_of_type t1 @ ids_of_type t2 in
  let unique_shape_vars = List.sort_uniq compare @@ List.flatten @@
    List.map (fun (ftyp, _) -> ids_of_type @@ ftyp.types) fns in

  let local_map = List.fold_left
    (fun table shape_var ->
      if StringMap.mem shape_var table then raise
          (Failure ("attempting to redefine already defined symbol: "
                    ^ shape_var))
      else StringMap.add shape_var (Nat) table) local_map' unique_shape_vars
  in
  (* Combine local map with enclosing map,
   * throwing away the redundant variables in enclosing scope *)
  StringMap.union (fun _key _v1 v2 -> Some v2) enclosing local_map

let rec list_of_type typ = match typ with
  | Bool -> [Bool] | Nat -> [Nat] | Tensor(s) -> [Tensor(s)]
  | Arrow(t1, t2) -> list_of_type t1 @ list_of_type t2
let but_last lst = List.rev @@ List.tl @@ List.rev lst

(* Create a local table for a single function's arguments *)
let build_local_table enclosing (ftyp, fdef) =
  let types = but_last @@
    list_of_type ftyp.types and params = fdef.fparams in
  let build_arg_map types params =
    List.fold_left2 (fun map typ param ->
    if StringMap.mem param map then raise (Failure
     ("Non-linear pattern match encountered in definition of symbol " ^ param))
    else StringMap.add param (typ) map) StringMap.empty types params in

  let local_map = build_arg_map types params in
  (* Combine local map with enclosing map,
   * throwing away the redundant variables in enclosing scope *)
  StringMap.union (fun _key _v1 v2 -> Some v2) enclosing local_map

(* Pretty printing *)
let string_of_table map =
  (String.concat "\n" @@
  List.map (fun (name, types) -> name ^ " : " ^ (string_of_typ types)
           ) (StringMap.bindings map)) ^ "\n"


let rec lookup_symb symb table =
    (* Find utility exists in ocaml 4.06 but Edwards doesn't have that version so
     * we write it again here *)
    let find' key map =
        try Some (StringMap.find key map)
        with Not_found -> None in
  match find' symb table with
    | None -> raise (Failure ("Encountered undefined symbol: " ^ symb))
    | Some typ_list -> typ_list

(* Tensor literal checking functions *)
let rec flatten expr = match expr with (TLit(l)) -> (match l with
  | Fliteral(_) :: _ -> l
  | _ -> List.flatten (List.map flatten l))
  | _ -> raise (Failure "can't flatten a non_tlit expr")

let rec build_shape expr = match expr with
  | Fliteral(_) -> []
  | TLit(l) -> List.length l :: (build_shape (List.hd l))
  | _ -> raise (Failure "Internal error:
                cannot call build_shape on non-tensor-literal expression")

let rec verify expr = match expr with (TLit(l)) -> (match List.hd l with
  | Fliteral(_) -> true
  | TLit(x) -> List.for_all (fun c -> (match c with
        | TLit(component) -> List.length component = List.length x
        | _ -> raise (Failure "Invalid entity in tensor literal"))) l
        && List.for_all (fun x -> x) (List.map verify l)
  | _ -> raise (Failure "Invalid entity in tensor literal"))
  | _ -> raise (Failure "internal error: can't verify non_tensor_literal")

let rec last_type = function
  | Arrow(_, ts) -> last_type ts
  | t -> t

(* Check expr: return sexpr or error *)
let rec check_expr expression table =
  let type_of expr = fst (check_expr expr table) in
  match (expression : expr) with
    | Literal(i) -> (Nat, SLiteral(i))
    | Fliteral(s) -> (Tensor([]), SFliteral(s))
    | BoolLit(b) -> (Bool, SBoolLit(b))
    | TLit(_) -> let t = verify expression in if t then
                 let shape = build_shape expression in
                 let components = flatten expression in
                 let unwrap_components = List.map (fun c -> match c with
                  | Fliteral(f) -> f
                  | _ -> raise (Failure "Internal error: non-float encounted in
                 tensor literal")
                 ) in
                 (Tensor(List.map (fun s -> ALiteral(s)) shape),
                 STLit(unwrap_components components, shape))
                 else raise (Failure "Invalid tensor literal")
    | Id(s) -> (lookup_symb s table, SId(s))
    | Unop(Neg, _expr) -> raise (Failure
          "You can't negate Nats and tensors haven't been implemented yet")
    | Aop(expr1, op, expr2) -> if type_of expr1 <> type_of expr2 then raise
        (Failure "Detected arithmetic operation on incompatible types") else
        begin
            match type_of expr1 with
                | Nat -> (Nat,
                        SAop((check_expr expr1 table), op, (check_expr expr2 table)))
                | Bool -> raise (Failure "Detected arithmetic operation on boolean")
                | Tensor([]) -> (Tensor([]),
                        SAop((check_expr expr1 table), op, (check_expr expr2 table)))
                | Tensor(_) -> raise (Failure "Not yet implemented")
                | Arrow(_,_) -> raise
                        (Failure "Arithmetic operation on partially applied function")
        end
    | Boolop(expr1, op, expr2) -> if type_of expr1 <> type_of expr2 then raise
        (Failure "Detected boolean operation on incompatible types") else
        begin
            match type_of expr1 with
            | Nat -> raise (Failure "Detected boolean operation on Nats")
            | Bool -> (Bool,
                    SBoolop((check_expr expr1 table), op, (check_expr expr2 table)))
            | Tensor(_) -> raise
                    (Failure "Detected boolean operation on incompatible types")
            | Arrow(_,_) -> raise
                    (Failure "Boolean operation on partially applied function")
        end
    | Rop(expr1, op, expr2) -> if type_of expr1 <> type_of expr2 then raise
        (Failure "Detected relational operation on incompatible types") else
        begin
            match type_of expr1 with
            | Nat -> (Bool,
                    SRop((check_expr expr1 table), op, (check_expr expr2 table)))
            | Bool -> raise (Failure "Detected relational operation on boolean")
            | Tensor(_) -> raise (Failure "Not yet implemented")
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
    | CondExpr(expr1, expr2, expr3) -> if type_of expr1 <> Bool
        then raise (Failure "Non-boolean expression in if statement")
        else if type_of expr2 <> type_of expr3 then raise
        (Failure "Incompatible types in conditional expressions") else
        (type_of expr2,
         SCondExpr(check_expr expr1 table,
                   check_expr expr2 table, check_expr expr3 table))
    | TensorIdx(_,_) -> raise (Failure "Not yet implemented")

(* If a function has a single type in its decl and the same
 * id appears in its definition raise error, else return true*)
let recursive_check (ftyp, fdef) =
  let {types=ftyp'; _} = ftyp in
  let single_typ = match ftyp' with
    | Nat | Bool | Tensor(_) -> true
    | Arrow(_) -> false in
  let {ftyp_name=fid; _} = ftyp in
  let rec rec_def fexpr = match fexpr with
    | Literal(_) | Fliteral(_) | BoolLit(_) -> false
    | TLit(e) -> List.for_all rec_def e
    | Unop(_, e) -> rec_def e
    | Aop(e1, _, e2) -> rec_def e1 && rec_def e2
    | Boolop(e1, _, e2) -> rec_def e1 && rec_def e2
    | Rop(e1, _, e2) -> rec_def e1 && rec_def e2
    | App(e1, e2) -> rec_def e1 && rec_def e2
    | CondExpr(e1, e2, e3) -> rec_def e1 && rec_def e2 && rec_def e3
    | TensorIdx(_, e) -> List.for_all rec_def e
    | Id(s) -> if s = fid then failwith "Recursively defined Nat/Bool/Tensor not permitted" else false in
  let {main_expr=main_expr'; _} = fdef in
  if single_typ && rec_def main_expr' then false else true

(* Check a single function - return sfunc or error *)
let rec check_func enclosing (ftyp, fdef) =
  let table' = build_local_table enclosing (ftyp, fdef) in
  let table  = build_fns_table table' fdef.scope in
  let this_sexpr = check_expr fdef.main_expr table in
  let sfparams = List.fold_right2 (fun typ arg acc -> (typ, arg)::acc)
    (list_of_type ftyp.types |> but_last) fdef.fparams [] in
  if recursive_check (ftyp, fdef) && last_type ftyp.types = fst this_sexpr then

    { sfname = ftyp.ftyp_name; stype = ftyp.types;
      sfparams = sfparams; sfexpr = this_sexpr;
      sscope = List.map (fun f -> check_func table f) fdef.scope }

    else raise (Failure ("Declared type " ^ string_of_typ ftyp.types
           ^ " but received type " ^ string_of_typ @@ fst this_sexpr))

(* Check entire program *)
let check (main_expr, funcs) =
  (* First build table of functions in global scope *)
  let global_table = build_fns_table StringMap.empty funcs in
  let check_main = check_expr main_expr global_table in
  match check_main with
    | Bool, _ | Nat, _ | Tensor(_), _ ->
            (check_main, List.map (fun f -> check_func global_table f) funcs)
    | _ -> raise (Failure "main must be of type Tensor, Nat, or Bool")

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
    if StringMap.mem fdef.fdef_name table then raise
          (Failure ("attempting to redefine already defined symbol: "
                    ^ fdef.fdef_name))
    else StringMap.add fdef.fdef_name (styp_of_typ ftyp.types) table) 
        base_map fns in

  (* Combine local map with enclosing map, 
   * throwing away the redundant variables in enclosing scope *)
  StringMap.union (fun _key _v1 v2 -> Some v2) enclosing local_map

(* Create a local table for a single function's arguments *)
let build_local_table enclosing (ftyp, fdef) = 
  let types = but_last @@ list_of_type ftyp.types 
  and params = fdef.fparams in

  (* TODO: refactor this garbage*)
  (* (1* Filter out any names declared with indices on the LHS of the function def *1) *)
  (* let args, indices = List.fold_right2 *) 
  (*   (fun param typ (arg, idx) -> *) 
  (*   if String.contains param '[' then *) 
  (*       let lst = (match String.split_on_char '[' param with *) 
  (*           | [a; is] -> [a; is] *)
  (*           | _ -> raise (Failure "Internal parsing error")) *)
  (*       in *)
  (*       let a = List.nth lst 0 and is = List.nth lst 1 in *)
  (*       let is = String.sub is 0 (String.length is - 1) in *)
  (*       let is = String.split_on_char ',' is in *) 
  (*       let bound = List.fold_right2 (fun str len acc -> *) 
  (*           let is_bound = List.assoc_opt str acc in *)
  (*               if is_bound = Some len || is_bound = None then *)
  (*                   (str, len) :: acc else *) 
  (*               raise (Failure ("Error: index " ^ str ^ "is already bound to shape " *)
  (*                   ^ string_of_int len ^ " and cannot be rebound."))) is *) 
  (*           (match typ with STensor(shape) -> shape | _ -> raise (Failure *)
  (*           "Invalid type: cannot index a non-tensor parameter.")) [] in *)
  (*       a :: arg, List.fold_right (fun ix ixs -> StringMap.add (fst ix) *) 
  (*       (match snd ix with ALiteral i -> i | _ -> failwith "error") ixs) bound idx *)
  (*   else param :: arg, idx *)
  (* ) params (types) ([], StringMap.empty) in *)

  let build_arg_map types params =
    List.fold_left2 (fun map typ param ->
    if StringMap.mem param map then raise (Failure
     ("Non-linear pattern match encountered in definition of symbol " ^ param))
    else StringMap.add param typ map) base_map types params in

  let local_map = build_arg_map types params in
  (* Add the indices, all bound to Nat type *)
  (* let local_map = StringMap.fold *)
  (*   (fun name _ acc -> StringMap.add name SNat acc) indices local_map in *)
  (* Combine local map with enclosing map, 
   * throwing away the redundant variables in enclosing scope *)
  StringMap.union (fun _key _v1 v2 -> Some v2) enclosing local_map

(* Pretty printing *)
let string_of_table map =
  (String.concat "\n" @@
  List.map (fun (name, types) -> name ^ " : " ^ (string_of_styp types)
           ) (StringMap.bindings map)) ^ "\n"


let rec lookup_symb symb table =
  match StringMap.find_opt symb table with
    | None -> raise (Failure ("Encountered undefined symbol: " ^ symb))
    | Some typ_list -> typ_list

(* Check expr: return sexpr or error *)
let rec check_expr expression table =
  let type_of expr = fst (check_expr expr table) in
  match (expression : expr) with
    | Literal(i) -> (SNat , SLiteral(i))
    | Fliteral(s) -> (STensor([]), SFliteral(s))
    | BoolLit(b) -> (SBool, SBoolLit(b))
    | TLit(_) -> let t = Infer.verify expression in if t then
                 let shape = Infer.build_shape expression in
                 let components = Infer.flatten expression in
                 let unwrap_components = List.map (fun c -> match c with
                  | Fliteral(f) -> f
                  | _ -> raise (Failure "Internal error: non-float encounted in
                 tensor literal")
                 ) in
                 (STensor(shape), STLit(unwrap_components components, shape))
                 else raise (Failure "Invalid tensor literal")
    | Id(s) -> (lookup_symb s table, SId(s))
    | Unop(Neg, _expr) -> raise (Failure
          "You can't negate Nats and tensors haven't been implemented yet")
    | Aop(expr1, op, expr2) -> if type_of expr1 <> type_of expr2 then raise
        (Failure "Detected arithmetic operation on incompatible types") else
        begin
            match type_of expr1 with
                | SNat -> (SNat,
                        SAop((check_expr expr1 table), op, (check_expr expr2 table)))
                | SBool -> raise (Failure "Detected arithmetic operation on boolean")
                | STensor([]) -> (STensor([]),
                        SAop((check_expr expr1 table), op, (check_expr expr2 table)))
                | STensor(_) -> raise (Failure "Not yet implemented")
                | SArrow(_,_) -> raise
                        (Failure "Arithmetic operation on partially applied function")
        end
    | Boolop(expr1, op, expr2) -> if type_of expr1 <> type_of expr2 then raise
        (Failure "Detected boolean operation on incompatible types") else
        begin
            match type_of expr1 with
            | SNat -> raise (Failure "Detected boolean operation on Nats")
            | SBool -> (SBool,
                    SBoolop((check_expr expr1 table), op, (check_expr expr2 table)))
            | STensor(_) -> raise
                    (Failure "Detected boolean operation on incompatible types")
            | SArrow(_,_) -> raise
                    (Failure "Boolean operation on partially applied function")
        end
    | Rop(expr1, op, expr2) -> if type_of expr1 <> type_of expr2 then raise
        (Failure "Detected relational operation on incompatible types") else
        begin
            match type_of expr1 with
            | SNat -> (SBool,
                    SRop((check_expr expr1 table), op, (check_expr expr2 table)))
            | SBool -> raise (Failure "Detected relational operation on boolean")
            | STensor(_) -> raise (Failure "Not yet implemented")
            | SArrow(_,_) -> raise
                        (Failure "Relational operation on partially applied function")
        end
    | App(expr1, expr2) ->
      begin
        match expr1 with
        | App(expr1', expr2') ->
          begin
            match type_of expr1' with
            | SArrow(first_param_type, remaining_types) ->
              if type_of expr2' = first_param_type
                 then (last_stype remaining_types, SApp(check_expr expr1' table,
                      check_expr expr2' table :: [check_expr expr2 table]))
              else
                 raise (Failure ("Expected type " ^ string_of_styp first_param_type
                       ^ " but instead received " ^ string_of_styp (type_of expr2)))
            | _ -> raise (Failure "Type error")
          end
        | _ ->
          begin
            match type_of expr1 with
            | SArrow(param_type, return_type) ->
              if type_of expr2 = param_type
                 then (return_type, SApp(check_expr expr1 table, [check_expr expr2 table]))
              else
                 raise (Failure ("Expected type " ^ string_of_styp param_type
                       ^ " but instead received " ^ string_of_styp (type_of expr2)))
            | _ -> raise (Failure "Type error")
          end
      end
    | CondExpr(expr1, expr2, expr3) -> if type_of expr1 <> SBool
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
  let ftyp' = ftyp.types in
  let single_typ = match ftyp' with
    | Nat | Bool | Tensor(_) -> true
    | Arrow(_) -> false in
  let fid = ftyp.ftyp_name in
  let rec rec_def fexpr = match fexpr with
    | Literal(_) | Fliteral(_) | BoolLit(_) | TLit(_) -> false
    | Unop(_, e) -> rec_def e
    | Aop(e1, _, e2) -> rec_def e1 && rec_def e2
    | Boolop(e1, _, e2) -> rec_def e1 && rec_def e2
    | Rop(e1, _, e2) -> rec_def e1 && rec_def e2
    | App(e1, e2) -> rec_def e1 && rec_def e2
    | CondExpr(e1, e2, e3) -> rec_def e1 && rec_def e2 && rec_def e3
    | TensorIdx(_, e) -> List.for_all rec_def e
    | Id(s) -> if s = fid then failwith "Recursively defined Nat/Bool/Tensor not permitted" else false in
  let main_expr = fdef.main_expr in
  if single_typ && rec_def main_expr then false else true

(* Check a single function - return sfunc or error *)
let rec check_func enclosing (ftyp, fdef) =
  let table' = build_local_table enclosing (ftyp, fdef) in
  let table  = build_fns_table table' fdef.scope in

  let this_sexpr = check_expr fdef.main_expr table in
  let sfparams = List.fold_right2 (fun typ arg acc -> (typ, arg)::acc)
    (list_of_type ftyp.types |> but_last) fdef.fparams [] in
  if recursive_check (ftyp, fdef) && last_type ftyp.types = fst this_sexpr then

    { sfname = ftyp.ftyp_name; stype = styp_of_typ ftyp.types;
      sindices = StringMap.empty; (* for now... *)
      slocals = []; (* also for now *)
      sfparams = sfparams; sfexpr = this_sexpr; 
      sscope = List.map (check_func table) fdef.scope }

    else raise (Failure ("Declared type " ^ string_of_typ ftyp.types
           ^ " but received type " ^ string_of_styp @@ fst this_sexpr)) 

(* Check entire program *)
let check (main_expr, funcs) =
  (* Infer all tensor shapes *)
  let funcs = Infer.deduce_shapes funcs in
  (* First build table of functions in global scope *)
  let global_table = build_fns_table StringMap.empty funcs in
  let check_main = check_expr main_expr global_table in
  match check_main with
    | SBool, _ | SNat, _ | STensor(_), _ -> 
            (check_main, List.map (fun f -> check_func global_table f) funcs)
    | _ -> raise (Failure "main must be of type Tensor, Nat, or Bool")

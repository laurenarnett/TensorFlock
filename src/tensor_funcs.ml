(* Utility functions for tensor checking *)
open Ast
open Sast
module StringMap = Map.Make (String)

(* Given an id, the number that it is set to, and an aexpr, reduce the aexpr *)
let rec eval env aexpr = match aexpr with
  | ALiteral num -> num
  | AId s -> (match StringMap.find_opt s env with
    | Some num -> num
    | None -> raise (Failure "Not enough information to deduce tensor shapes"))
  | AAop(e1, Add, e2)  -> eval env e1 + eval env e2
  | AAop(e1, Sub, e2)  -> eval env e1 - eval env e2
  | AAop(e1, Mult, e2) -> eval env e1 * eval env e2
  | AAop(e1, Div, e2)  -> eval env e1 / eval env e2
  | AAop(e1, Mod, e2)  -> eval env e1 mod eval env e2
  | AAop(e1, Expt, e2) -> let pow base expt =
      let rec pow' res base expt =
          if expt = 0 then res else pow' (res * base) base (expt - 1) in
             pow' 1 base expt in
        pow (eval env e1) (eval env e2)

(* Given a sprogram, collect all tensor literal shapes and bind then to ids so
 * that we can reduce all shape expressions *)
let rec env_of_sfuncs sfuncs = 
  let env_of_sfunc env sfunc = match sfunc.sfexpr with 
    | Tensor(shape), STLit(_, ints) ->
      List.fold_left2 (fun map dim num ->
          (match dim with
          | ALiteral _ -> env
          | AId s -> (match StringMap.find_opt s env with 
              | None -> StringMap.add s num map
              | Some num' -> if num <> num' then 
                  failwith (s ^ " is already bound to " ^
                            string_of_int num' ^ " and cannot be rebound.")
                else env)
          | _ -> failwith "Cannot have arbitrary polynomial
                            as the shape of a tensor literal")
        ) env shape ints
    | _ -> env in
  let combine_envs env_list = List.fold_left (fun acc map ->
        (StringMap.union (fun id s1 s2 -> 
          if s1 = s2 then Some s2 else failwith 
          (id ^ " is already bound to " ^ string_of_int s1 ^
          ". Cannot bind it to " ^ string_of_int s2 ^ " as well.")) acc map)
        ) StringMap.empty env_list in
        
  combine_envs @@ 
    List.map (env_of_sfunc StringMap.empty) sfuncs @
    (* Recurse over all inner scopes *)
    List.map (fun sf -> env_of_sfuncs sf.sscope) sfuncs

let rec deduce_shapes sfuncs = 
    let env = env_of_sfuncs sfuncs in
    (* replace_type : env -> sfunc -> sfunc *)
    let rec replace_type env stype = match stype with
      | Nat | Bool | Tensor [] -> stype
      | Tensor shape -> Tensor (List.map (fun s -> ALiteral(eval env s)) shape)
      | Arrow(t1, t2) -> Arrow(replace_type env t1, replace_type env t2) in
    List.map (fun sf -> {sf with stype = replace_type env sf.stype; 
                 sscope = deduce_shapes sf.sscope }) sfuncs

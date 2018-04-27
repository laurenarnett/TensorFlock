open Sast
open Semant
module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

(* Function to rename all of the IDs in a sexpr *)
let rec rename old_name new_name sexpr = match sexpr with
  | SLiteral(_) -> sexpr
  | SFliteral(_) -> sexpr
  | SBoolLit(_) -> sexpr
  | STLit(_) -> sexpr
  | SId(s) -> if s = old_name then SId new_name else SId s
  | SUnop(o, e) -> SUnop(o, (fst e, rename old_name new_name (snd e)))
  | SAop((t1, e1), o, (t2, e2)) ->
    SAop((t1, rename old_name new_name e1), o,
         (t2, rename old_name new_name e2))
  | SBoolop((t1, e1), o, (t2, e2)) ->
    SBoolop((t1, rename old_name new_name e1), o,
            (t2, rename old_name new_name e2))
  | SRop((t1, e1), o, (t2, e2)) ->
    SRop((t1, rename old_name new_name e1), o,
         (t2, rename old_name new_name e2))
  | SApp((t, e), es) ->
    SApp((t, rename old_name new_name e),
         List.map (fun sx -> (fst sx, rename old_name new_name (snd sx))) es)
  | SCondExpr((t1, e1), (t2, e2), (t3, e3)) ->
    SCondExpr((t1, rename old_name new_name e1),
              (t2, rename old_name new_name e2),
              (t3, rename old_name new_name e3))
  | STensorIdx(_, _) -> raise (Failure "Not yet imlemented")

(* The new_id function takes an identifier and returns a new one. The method of
 * dealing with a mutable counter was taken from
 * http://typeocaml.com/2015/01/20/mutable/*)
let new_id =
  let helper () =
    let count = ref 0 in
    let new_id' old_id' =
    count := !count + 1;
    old_id' ^ "~" ^ string_of_int !count in
    new_id' in helper()

(* Given a set of names in the enclosing scope, traverse the params and inner
 * function definitions to see if shadowing occurs and renaming needs to be
 * done *)
let rename_sfunc enclosing sfunc =
  (* Edit the function parameters if they shadow and make a list of ones that
   * we need to change inside of the expressions *)
  let (to_change', new_params) = List.fold_right
      (fun (typ, name) (diff, ps) -> if StringSet.mem name enclosing then
          let new_name = new_id name in
          (StringMap.add name new_name diff, (typ, new_name) :: ps)
        else (diff, (typ, name) :: ps)) sfunc.sfparams (StringMap.empty, []) in
  (* Edit the names of the functions defined inside of the inner scope.
   * Overwrite the names of the variables that we need to change inside of the
   * expressions since funciton names defined in inner scopes can shadow
   * parameters *)
  let (to_change, new_sfnames) = List.fold_right
      (fun sf (diff, names) -> let name = sf.sfname in
        if StringSet.mem name enclosing then
        let new_name = new_id name in
          (StringMap.add name new_name diff, new_name :: names)
        else (diff, name :: names)) sfunc.sscope (to_change', []) in

  { sfunc with
      sfparams = new_params;
      sscope = List.map2
          (fun sf name -> { sf with sfname = name }) sfunc.sscope new_sfnames;
      sfexpr = (fst sfunc.sfexpr),
          StringMap.fold (fun oldn newn expr -> rename oldn newn expr)
          to_change (snd sfunc.sfexpr)
  }

(* Rename all of the sfuncs in a sprogram. NOTE: we do not need to rename the
 * main expression, since it only has access to things defined in the top
 * scope, which never get renamed. *)
let rename_sprogram (main_expr, sfuncs) =
    let rec rename_sfuncs sfuncs enclosing =
        let enclosing' = List.fold_right StringSet.add (List.map (fun sf ->
            sf.sfname) sfuncs) enclosing in
        List.map (fun sf ->
          let enclosing'' =
            List.fold_right StringSet.add (List.map snd sf.sfparams) enclosing' in
            rename_sfunc enclosing'' sf) sfuncs in
    main_expr, rename_sfuncs sfuncs StringSet.empty

(* Taken from semant, primed with "cast" *)
let base_map = StringMap.singleton "cast" (SArrow(SNat,STensor([])))

let build_fns_table enclosing sfuncs =
  let local_map = List.fold_left (fun table sfunc ->
    StringMap.add sfunc.sfname sfunc.stype table) base_map sfuncs in
  StringMap.union (fun _key _v1 v2 -> Some v2) enclosing local_map

(* Take in a sfunc and its enclosing scope, and return the sfunc
 * with all its free variables lifted into params *)
let lift_params enclosing sfunc =
  let param_ids = List.fold_left
    (fun lst (_, s) -> s :: lst) [] sfunc.sfparams in
  let rec get_free_vars (e : sexpr) acc = match e with
    | (_,SLiteral(_)) | (_, SFliteral(_))
    | (_, SBoolLit(_)) | (_, STLit(_)) -> acc
    | (_, SUnop(_, e)) -> get_free_vars e acc
    | (_, SAop(e1, _, e2)) -> let lst = get_free_vars e1 acc in
      get_free_vars e2 lst
    | (_, SBoolop(e1, _, e2)) -> let lst = get_free_vars e1 acc
      in get_free_vars e2 lst
    | (_, SRop(e1, _, e2)) -> let lst = get_free_vars e1 acc
      in get_free_vars e2 lst
    | (_, SApp(e1, e2)) -> let lst = get_free_vars e1 acc in
      List.fold_left (fun lst' sexpr -> get_free_vars sexpr lst') lst e2
    | (_, SCondExpr(e1, e2, e3)) -> let lst = get_free_vars e1 acc in
      let lst' = get_free_vars e2 lst in get_free_vars e3 lst'
    | (_, STensorIdx(_, _e)) -> get_free_vars e acc
    | (styp, SId(s)) -> if StringMap.mem s enclosing &&
      not (List.mem s param_ids)
      then (s, styp) :: acc else acc in
  let free_vars = get_free_vars sfunc.sfexpr [] in
  List.fold_left (fun sfunc' (id, styp) ->
    { sfname = sfunc'.sfname;
      stype = sfunc'.stype;
      sfparams = (styp, id) :: sfunc'.sfparams;
      sindices = sfunc'.sindices;
      slocals = (styp, id) :: sfunc'.sfparams;
      sfexpr = sfunc'.sfexpr;
      sscope = sfunc'.sscope })
    sfunc free_vars


let lift_params_sprogram (_main_sexpr, sfuncs) =
  let global_table = build_fns_table StringMap.empty sfuncs in
  let _lifted_sfuncs = List.fold_left (fun lst sfunc ->
    (lift_params global_table sfunc) :: lst) [] sfuncs in
  _lifted_sfuncs


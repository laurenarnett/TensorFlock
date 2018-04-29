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
  | STensorIdx((t, e), _) -> rename old_name new_name e
  | Forall r -> rename old_name new_name (snd r.sexpr)
  | Contract r -> rename old_name new_name (snd r.sexpr)

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

(* Takes in a list of sfuncs, and returns a string map of
 * Key: ids
 * Value: stype *)
let build_fns_table enclosing sfuncs =
  let local_map = List.fold_left (fun table sfunc ->
    StringMap.add sfunc.sfname sfunc.stype table) base_map sfuncs in
  StringMap.union (fun _key _v1 v2 -> Some v2) enclosing local_map

let build_locals_table enclosing sfunc =
  let params = List.rev @@ List.tl @@ List.rev sfunc.sfparams in
  let local_map = List.fold_left
    (fun table (styp, id) -> StringMap.add id styp table) base_map params in
  StringMap.union (fun _key _v1 v2 -> Some v2) enclosing local_map

(* Replace the func with the matching id, return
 * an updated sprogram *)
let rec replace_sfunc sfunc sfuncs = List.fold_left
  (fun lst sfunc' ->
    (* First check if the func def is in a sscope *)
    let sscope' = replace_sfunc sfunc sfunc.sscope in
    (* Replaces a sfunc with one that has lifted params. If the name
     * of the sfunc passed in matches the name that we're trying to replace,
     * then replace it, else return the sfunc that was passed in *)
    if sfunc'.sfname = sfunc.sfname then
    { sfname = sfunc.sfname;
      stype = sfunc.stype;
      sfparams = sfunc.sfparams;
      sindices = sfunc.sindices;
      slocals = sfunc.slocals;
      sfexpr = sfunc.sfexpr;
      (* replace with the scope above *)
      sscope = sscope' } :: lst
    else
    { sfname = sfunc'.sfname;
      stype = sfunc'.stype;
      sfparams = sfunc'.sfparams;
      sindices = sfunc'.sindices;
      slocals = sfunc'.slocals;
      sfexpr = sfunc'.sfexpr;
      (* replace with the scope above *)
      sscope = sscope' } :: lst ) [] sfuncs



(* Take in a sexpr and return a list of ids inside that sexpr *)
let rec get_ids (sexper : Sast.sexpr) acc = match sexper with
  | (_,SLiteral(_)) | (_, SFliteral(_))
  | (_, SBoolLit(_)) | (_, STLit(_)) -> acc
  | (_, SUnop(_, e)) -> get_ids e acc
  | (_, SAop(e1, _, e2)) -> let lst = get_ids e1 acc in
    get_ids e2 lst
  | (_, SBoolop(e1, _, e2)) -> let lst = get_ids e1 acc
    in get_ids e2 lst
  | (_, SRop(e1, _, e2)) -> let lst = get_ids e1 acc
    in get_ids e2 lst
  | (_, SApp(e1, e2)) -> let lst = get_ids e1 acc in
    List.fold_left (fun lst' sexpr -> get_ids sexpr lst') lst e2
  | (_, SCondExpr(e1, e2, e3)) -> let lst = get_ids e1 acc in
    let lst' = get_ids e2 lst in get_ids e3 lst'
  | (_, STensorIdx(_, _)) -> acc
  | (styp, SId(s)) -> (s, styp) :: acc

let get_first_n lst n = List.rev @@ List.fold_left
  (fun acc el -> if List.length acc = n then acc else el :: acc) [] lst

let update_sapp (t, SApp(e1, e2)) sfunc =
  let (app_id, _) = List.hd @@ get_ids e1 [] in
  let params_len = List.length sfunc.sfparams in
  let args_len = List.length e2 in
  (* check if the ids match and if any params have been lifted *)
  if app_id = sfunc.sfname && params_len  > args_len
    then let ids_of_params = List.fold_right
      (fun (styp, id) lst -> (styp, SId(id)) :: lst) sfunc.sfparams [] in
    let no_of_new_params = List.length sfunc.sfparams - List.length e2 in
    let updated_args = get_first_n ids_of_params no_of_new_params in
    (t, SApp(e1, updated_args))
  else if params_len < args_len then
    failwith ("Internal Error: number of parameters in a lifted function "
      ^ "are less than the number of arguments in its call site.")
  else (t, SApp(e1, e2))

(* Takes a single lifted sfunc, a list of sfuncs, and returns
 * a new list of sfuncs with updated call sites for the sfunc *)
let rec update_call_sites sfunc sfuncs = List.fold_left
  (fun acc sfunc' ->
    let updated_sscope = update_call_sites sfunc sfunc.sscope in
    let rec update_sexpr sexpr = match sexpr with
    | (t, SLiteral(i)) -> (t, SLiteral(i))
    | (t, SFliteral(f)) -> (t, SFliteral(f))
    | (t, SBoolLit(b)) -> (t, SBoolLit(b))
    | (t, STLit(c, d)) -> (t, STLit(c, d))
    | (t, SId(s)) -> (t, SId(s))
    | (t, SUnop(u, e)) -> (t, SUnop(u, update_sexpr e))
    | (t, SAop(e1, a, e2)) -> (t, SAop(update_sexpr e1, a, update_sexpr e2))
    | (t, SBoolop(e1, b, e2)) ->
      (t, SBoolop(update_sexpr e1, b, update_sexpr e2))
    | (t, SRop(e1, r, e2)) -> (t, SRop(update_sexpr e1, r, update_sexpr e2))
    | (t, SCondExpr(e1, e2, e3)) ->
      (t, SCondExpr(update_sexpr e1, update_sexpr e2, update_sexpr e3))
    (* Tensor indices can not have function application, hence no update *)
    | (t, STensorIdx(e, c)) -> (t, STensorIdx(e, c))
    | (t, SApp(e1, e2)) -> update_sapp (t, SApp(e1, e2)) sfunc in
    {sfname = sfunc'.sfname;
     stype = sfunc'.stype;
     sfparams = sfunc'.sfparams;
     sindices = sfunc'.sindices;
     slocals = sfunc'.slocals;
     sfexpr = update_sexpr sfunc'.sfexpr;
     sscope = updated_sscope; } :: acc) [] sfuncs

(* Get a list of ids from a list of sfuncs *)
let get_func_ids sfuncs = List.fold_left
  (fun lst sfunc -> sfunc.sfname :: lst) [] sfuncs

(* Take in a sfunc and its enclosing scope, and return the sfunc
 * with all its free variables lifted into params *)
let rec lift_params enclosing sfunc sfuncs =
  let sexpr_id_typs = get_ids sfunc.sfexpr [] in
  let local_map = build_locals_table enclosing sfunc in
  (* Build up four sets from which we'll derive the free vars:
   * local_ids \ (enclosing_ids U sscope_func_ids U param_ids) *)
  let sexpr_ids = StringSet.of_list @@ List.fold_left
    (fun lst (id, _) -> id :: lst) [] sexpr_id_typs in
  let enclosing_ids = StringSet.of_list @@ StringMap.fold
    (fun k _ lst -> k :: lst) enclosing [] in
  let sscope_func_ids = StringSet.of_list @@ get_func_ids sfunc.sscope in
  let param_ids = StringSet.of_list @@ List.fold_left
    (fun lst (_, s) -> s :: lst) [] sfunc.sfparams in
  let in_scope_ids = StringSet.union enclosing_ids
    (StringSet.union sscope_func_ids param_ids) in
  let free_vars_ids = StringSet.filter
    (fun id -> StringSet.mem id in_scope_ids) sexpr_ids in
  (* Rejoin free var ids with their styps *)
  let free_vars = StringSet.fold
    (fun id lst -> let styp = StringMap.find id local_map in
      (id, styp) :: lst) free_vars_ids [] in
  let lifted_sfunc = List.fold_left (fun sfunc' (id, styp) ->
    let lifted_sscope = List.fold_left
      (fun sfscope sfunc'' ->
        (lift_params local_map sfunc'' sfscope)) sfunc'.sscope
        sfunc'.sscope in
    { sfname = sfunc'.sfname;
      stype = sfunc'.stype;
      sfparams = (styp, id) :: sfunc'.sfparams;
      sindices = sfunc'.sindices;
      slocals = (styp, id) :: sfunc'.sfparams;
      sfexpr = sfunc'.sfexpr;
      sscope = lifted_sscope }) sfunc free_vars in
  (* Now rebuild sfuncs with the lifted sfunc *)
  let updated_sfuncs = replace_sfunc lifted_sfunc sfuncs in
  (* Return a new sprogram with updated call sites *)
  update_call_sites lifted_sfunc updated_sfuncs

let lift_params_sprogram (main_sexpr, sfuncs) =
  let global_table = build_fns_table StringMap.empty sfuncs in
  let lifted_sfuncs = List.fold_left (fun sfuncs' sfunc ->
    lift_params global_table sfunc sfuncs') sfuncs  sfuncs in
  (main_sexpr, lifted_sfuncs)


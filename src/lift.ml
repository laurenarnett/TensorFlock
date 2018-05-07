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

  let update_sfunc_sexpr sfunc =  (fst sfunc.sfexpr),
          StringMap.fold (fun oldn newn expr -> rename oldn newn expr)
          to_change (snd sfunc.sfexpr) in

  let updated_sscope' = List.map2
          (fun sf name -> { sf with sfname = name }) sfunc.sscope new_sfnames in

  let updated_sscope = List.map
    (fun sfunc' -> {sfunc' with sfexpr = update_sfunc_sexpr sfunc';})
    updated_sscope' in

  { sfunc with
      sfparams = new_params;
      (*sscope = List.map2*)
          (*(fun sf name -> { sf with sfname = name }) sfunc.sscope new_sfnames;*)
      sscope = updated_sscope;
      (*sfexpr = (fst sfunc.sfexpr),*)
          (*StringMap.fold (fun oldn newn expr -> rename oldn newn expr)*)
          (*to_change (snd sfunc.sfexpr)*)
      sfexpr = update_sfunc_sexpr sfunc;
  }

(* Rename all of the sfuncs in a sprogram. NOTE: we do not need to rename the
 * main expression, since it only has access to things defined in the top
 * scope, which never get renamed. *)
let rename_sprogram (main_expr, sfuncs) =
    let rec rename_sfuncs sfuncs enclosing =
      (* this adds the current sfunc to the enclosing list *)
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
  if List.length sfunc.sfparams = 0 then enclosing else
  let params = List.rev @@ List.tl @@ List.rev sfunc.sfparams in
  let sscope_locals = List.fold_left
    (fun table sfunc' -> StringMap.add sfunc'.sfname sfunc'.stype table)
    StringMap.empty sfunc.sscope in
  let local_map' = List.fold_left
    (fun table (styp, id) -> StringMap.add id styp table) base_map params in
  let local_map =
    StringMap.union (fun _key _v1 v2 -> Some v2) local_map' sscope_locals in
  StringMap.union (fun _key _v1 v2 -> Some v2) enclosing local_map

(* Replace the func with the matching id, return
 * an updated sprogram *)
let rec replace_sfunc sfunc sfuncs = List.map
    (* First check if the func def is in a sscope *)
    (* its failing below *)
    (* Replaces a sfunc with one that has lifted params. If the name
     * of the sfunc passed in matches the name that we're trying to replace,
     * then replace it, else return the sfunc that was passed in *)
  (fun sfunc' -> if sfunc'.sfname = sfunc.sfname then sfunc else sfunc') sfuncs

(* Take in a sexpr and return a list of ids inside that sexpr *)
let rec get_ids (sexper : Sast.sexpr) acc = match sexper with
  | (_, SLiteral(_)) | (_, SFliteral(_))
  | (_, SBoolLit(_)) | (_, STLit(_)) -> acc
  | (_, SUnop(_, e)) -> get_ids e acc
  | (_, SAop(e1, _, e2)) -> let lst = get_ids e1 acc in
    get_ids e2 lst
  | (_, SBoolop(e1, _, e2)) -> let lst = get_ids e1 acc
    in get_ids e2 lst
  | (_, SRop(e1, _, e2)) -> let lst = get_ids e1 acc
    in get_ids e2 lst
  (* Exclude the id of SApps, functions can't be passed as parameters *)
  | (_, SApp(_, e2)) ->
      List.fold_left (fun lst sexpr -> get_ids sexpr lst) acc e2
  | (_, SCondExpr(e1, e2, e3)) -> let lst = get_ids e1 acc in
    let lst' = get_ids e2 lst in get_ids e3 lst'
  | (_, STensorIdx(_, _)) -> acc
  | (styp, SId(s)) -> (s, styp) :: acc

let get_first_n lst n = List.rev @@ List.fold_left
  (fun acc el -> if List.length acc = n then acc else el :: acc) [] lst

(* This basically takes in an SApp and an updated sfunc, and modifies
 * the SApp to have the same args and type as the lifted params in sfunc *)
let update_sapp sexpr sfunc = match sexpr with
  | (t, SApp(e1, e2)) ->
    let (app_id, _) = List.hd @@ get_ids e1 [] in
    let params_len = List.length sfunc.sfparams in
    let args_len = List.length e2 in
    (*print_endline @@ "app id: " ^ app_id ^ " sfunc id: " ^ sfunc.sfname;*)
    (* check if the ids match and if any params have been lifted *)
    if app_id = sfunc.sfname (*&& params_len > args_len *) then
      let ids_of_params =
    (*print_endline "we have an sapp update";*)
        List.fold_right
        (fun (styp, id) lst -> (styp, SId(id)) :: lst) sfunc.sfparams [] in

      let no_of_new_params = params_len - args_len  in
      let updated_args = (get_first_n ids_of_params no_of_new_params) @ e2 in
      (t, SApp(e1, updated_args))
    else if app_id =  sfunc.sfname && params_len < args_len then
      failwith ("Internal Error: number of parameters in a lifted function "
        ^ "are less than the number of arguments in its call site.")
    else (t, SApp(e1, e2))
  | (_, (SLiteral _|SBoolLit _|SFliteral _|STLit (_, _)|SId _)) -> sexpr
  | _ -> sexpr

(* Takes a single lifted sfunc, a list of sfuncs, and returns
 * a new list of sfuncs with updated call sites for the sfunc *)
let rec update_call_sites sfunc sfuncs = List.map
  (fun sfunc' ->
      (*print_endline ("lifted sfunc: " ^ sfunc.sfname ^ " sfunc to update " ^*)
        (*sfunc'.sfname );*)
    (* First, recursively update sscope *)
    let updated_sscope = update_call_sites sfunc sfunc'.sscope in
    (* Then check a sexpr to see if it is an sapp and call update on it *)
    let rec update_sexpr sexpr = match sexpr with
    | (_, (SLiteral _|SBoolLit _|SFliteral _|STLit (_, _)|SId _)) -> sexpr
    (* Tensor indices can not have function application, hence no update *)
    | (_, STensorIdx(_, _)) -> sexpr
    | (t, SUnop(u, e)) -> (t, SUnop(u, update_sexpr e))
    | (t, SAop(e1, a, e2)) -> (t, SAop(update_sexpr e1, a, update_sexpr e2))
    | (t, SBoolop(e1, b, e2)) ->
      (t, SBoolop(update_sexpr e1, b, update_sexpr e2))
    | (t, SRop(e1, r, e2)) -> (t, SRop(update_sexpr e1, r, update_sexpr e2))
    | (t, SCondExpr(e1, e2, e3)) ->
      (t, SCondExpr(update_sexpr e1, update_sexpr e2, update_sexpr e3))
    | (t, SApp(e1, e2)) -> if sfunc.sfname = sfunc'.sfname then
      update_sapp (t, SApp(e1, e2)) sfunc else (t, SApp(e1, e2)) in
    (* return an updated sfunc *)
    { sfunc' with
      sfexpr = update_sexpr sfunc'.sfexpr;
      sscope = updated_sscope; } ) sfuncs

(* Get a list of ids from a list of sfuncs *)
let get_func_ids sfuncs = List.fold_left
  (fun lst sfunc -> sfunc.sfname :: lst) [] sfuncs

let params_to_stringmap (params : (Sast.styp * string) list) = List.fold_right
  (fun (typ, id) map -> StringMap.add id typ map) params StringMap.empty

(* Take in a sfunc and its enclosing scope, and return the sfunc
 * with all its free variables lifted into params *)
(* lift ids that are params of a parent, and not params of the current sfunc *)
let rec lift_params parental_params sfunc sfuncs =
  (* make a list of (id * styp)'s, used to create a map of ids to types *)
  let sexpr_id_typs : (string * Sast.styp) list = get_ids sfunc.sfexpr [] in
  let local_map = List.fold_left
    (fun map (id, styp) -> StringMap.add id styp map) StringMap.empty sexpr_id_typs  in
  (* make a map of the current parameters *)
  let current_params = params_to_stringmap sfunc.sfparams in
  (* this is a string map of any parents params unioned with current params,
   * to be used in the recursive on sfunc.sscope *)
  let enclosing_params = StringMap.union (fun _k _v1 v2 -> Some v2)
  parental_params current_params in
  let lifted_sscope = List.fold_left
    (fun sfscope sfunc' ->
      (lift_params enclosing_params sfunc' sfscope)) sfunc.sscope
      sfunc.sscope in

  (* this creates the three sets that will be used to determine if a
   * free var should be listed *)
  let sexpr_ids = StringSet.of_list @@ List.fold_left
    (fun lst (id, _) -> id :: lst) [] sexpr_id_typs in
  let enclosing_ids = StringSet.of_list @@ StringMap.fold
    (fun k _ lst -> k :: lst) parental_params [] in
  let param_ids = StringSet.of_list @@ List.fold_left
    (fun lst (_, s) -> s :: lst) [] sfunc.sfparams in
  (* this is the business end of lifting, this sets the conditions to decide
   * if an id gets lifted to a parameter *)
  let free_vars_ids = StringSet.filter
    (fun id -> not (StringSet.mem id param_ids)
               && (StringSet.mem id enclosing_ids)) sexpr_ids in
  (*Debugging*)
  (*print_endline "=========================";*)
  (*print_endline ("sfunc: " ^ sfunc.sfname);*)
  (*print_endline "Local map:";*)
  (*StringMap.iter (fun k v -> print_endline ("key: " ^ k ^ " val: " ^*)
  (*Sast.string_of_styp v)) local_map;*)
  (*print_endline "sexpr ids:";*)
  (*StringSet.iter (fun el -> print_endline el) sexpr_ids;*)
  (*print_endline "enclosing ids:";*)
  (*StringSet.iter (fun el -> print_endline el) enclosing_ids;*)
  (*print_endline "param ids:";*)
  (*StringSet.iter (fun el -> print_endline el) param_ids;*)
  (*print_endline "free var ids:";*)
  (*StringSet.iter (fun el -> print_endline el) free_vars_ids;*)

  (* Rejoin free var ids with their styps *)
  let free_vars = if StringSet.cardinal free_vars_ids > 0 then
    StringSet.fold
    (fun id lst -> let styp = StringMap.find id local_map in
    (id, styp) :: lst) free_vars_ids []
    else []
  in
  (*print_endline "lifted sfunc:";*)
  (*print_endline @@ Sast.string_of_sfunc lifted_sfunc;*)
  (*print_endline "Now manually mess with it:";*)
  let updated_stype = List.fold_left
    (fun stype' (_, styp) -> SArrow(styp, stype')) sfunc.stype free_vars in
  let updated_sfparams = List.fold_left
    (fun sfparams' (id, styp) -> (styp, id) :: sfparams' ) sfunc.sfparams
    free_vars in

  (* do a check here to update call sites in sexpr for functions defined in
* sscope*)
  let sscope_sfunc_decls = List.fold_left
    (fun acc sfunc -> match sfunc.stype with
      | SArrow(_, _) -> (*print_endline (Sast.string_of_sfunc sfunc);*) sfunc :: acc
      | _ -> acc) [] lifted_sscope in

  let rec update_sexpr_from_sfunc sexpr sfunc = match sexpr with
    | (_, (SLiteral _|SBoolLit _|SFliteral _|STLit (_, _)|SId _)) -> sexpr
    (* Tensor indices can not have function application, hence no update *)
    | (_, STensorIdx(_, _)) -> sexpr
    | (t, SUnop(u, e)) -> (t, SUnop(u, update_sexpr_from_sfunc e sfunc))
    | (t, SAop(e1, a, e2)) -> (t, SAop(update_sexpr_from_sfunc e1 sfunc, a,
    update_sexpr_from_sfunc e2 sfunc))
    | (t, SBoolop(e1, b, e2)) ->
      (t, SBoolop(update_sexpr_from_sfunc e1 sfunc, b, update_sexpr_from_sfunc
      e2 sfunc))
    | (t, SRop(e1, r, e2)) -> (t, SRop(update_sexpr_from_sfunc e1 sfunc, r,
    update_sexpr_from_sfunc e2 sfunc))
    | (t, SCondExpr(e1, e2, e3)) ->
      (t, SCondExpr(update_sexpr_from_sfunc e1 sfunc, update_sexpr_from_sfunc e2
      sfunc, update_sexpr_from_sfunc e3 sfunc))
    | (t, SApp(e1, e2)) -> update_sapp (t, SApp(e1, e2)) sfunc in

  let updated_sfexpr = List.fold_left
    (fun sexpr' sfunc' -> update_sexpr_from_sfunc sexpr' sfunc') sfunc.sfexpr
    sscope_sfunc_decls in

  let lifted_sfunc = {sfunc with
                 stype = updated_stype;
                 sfparams = updated_sfparams;
                 sfexpr = updated_sfexpr;
                 sscope = lifted_sscope;} in
  (*print_endline @@ Sast.string_of_sfunc lifted_sfunc;*)
  (* Now rebuild sfuncs with the lifted sfunc *)
  let updated_sfuncs = replace_sfunc lifted_sfunc sfuncs in
  (*print_endline "updated_sfuncs";*)
  (*List.iter (fun f -> print_endline @@ Sast.string_of_sfunc f) updated_sfuncs;*)
  (*print_endline "+++++++++++++++++++++++++";*)
  (* Return a new sprogram with updated call sites *)
  update_call_sites lifted_sfunc updated_sfuncs

(* Take in a list of sfuncs, and partition it into two lists of
 * sfunc decls, and everything else: (decls, vars) *)
let get_sfunc_decls sfuncs = List.fold_left
  (fun (decls, vars) sfunc -> match sfunc.stype with
  | SArrow(_, _) -> (sfunc :: decls, vars)
  | _ -> (decls, sfunc :: vars) ) ([], []) sfuncs

let rec block_float sfuncs acc = List.fold_left
    (fun lst sfunc ->
      let (collected_decls, sscope_sfuncs) = get_sfunc_decls sfunc.sscope in
      {sfunc with sscope = sscope_sfuncs; } :: (lst @ collected_decls)) acc sfuncs

let lift_sprogram (main_sexpr', sfuncs') =
  let (main_sexpr, sfuncs) = rename_sprogram (main_sexpr', sfuncs') in
  (*let global_table = build_fns_table StringMap.empty sfuncs in*)
  let lifted_sfuncs = List.fold_left (fun sfuncs' sfunc ->
    lift_params StringMap.empty sfunc sfuncs') sfuncs sfuncs in
  let block_floated_sfuncs = block_float lifted_sfuncs [] in
  (main_sexpr, block_floated_sfuncs)

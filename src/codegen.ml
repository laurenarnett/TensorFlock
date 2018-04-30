module L = Llvm
module A = Ast
open Sast
open Semant

module StringMap = Map.Make(String)
let context = L.global_context ()
let the_module = L.create_module context "TensorFlock"

(* Declare the LLVM versions of all built in types *)
let nat_t = L.i32_type context
let bool_t = L.i1_type context
let i8_t = L.i8_type context
let float_t = L.double_type context
let tensor_t = L.struct_type context [|
    nat_t; (* size: number of doubles the tensor holds *)
    nat_t; (* rank *)
    L.pointer_type nat_t; (* shape *)
    L.pointer_type nat_t; (* number of references *)
    L.pointer_type float_t; (* contents *)
  |]

let rec ltype_of_styp = function 
    SNat -> nat_t
  | SBool -> bool_t
  | STensor([]) -> float_t
  | STensor(_) -> L.pointer_type tensor_t
  (* All remaining types are arrow types *)
  | ts -> let rec list_of_styp = function
                | SNat -> [SNat] | SBool -> [SBool] | STensor(s) -> [STensor(s)]
                | SArrow(t1, t2) -> list_of_styp t1 @ list_of_styp t2 in
          L.function_type (List.rev (list_of_styp ts) |> List.hd |> ltype_of_styp) 
          (list_of_styp ts |> but_last |> List.map ltype_of_styp |> Array.of_list)


let printf_t = L.var_arg_function_type nat_t [| L.pointer_type i8_t |]
let printf_func = L.declare_function "printf" printf_t the_module

let talloc_t = L.function_type (L.pointer_type tensor_t) 
    [| nat_t; (* size *)
       L.pointer_type nat_t; (* shape *)
       L.pointer_type float_t (* contents *)
    |]
let talloc_func = L.declare_function "talloc" talloc_t the_module

let tdelete_t = L.function_type nat_t [| L.pointer_type tensor_t |]
let tdelete_func = L.declare_function "tdelete" tdelete_t the_module

let print_tensor_t = L.function_type nat_t [| L.pointer_type tensor_t |]
let print_tensor_func = L.declare_function "print_tensor" print_tensor_t the_module

(* If this throws an error, then something is actually problematic and we have
 * a real bug. *)
let lookup name map = StringMap.find name map

let dump_env map = StringMap.iter (fun name _value -> print_endline
    (name ^ " bound to " ^ ""
    (* (L.string_of_llvalue value) *)
    )) map

let rec codegen_sexpr (typ, detail) map builder = 
  let cond_expr pred cons alt = 
      (* Wholesale copying of the Kaleidescope tutorial's conditional
       * expression codegen *)
      let cond = codegen_sexpr pred map builder in
      (* Grab the first block so that we might later add the conditional branch
       * to it at the end of the function. *)
      let start_bb = L.insertion_block builder in
      let the_function = L.block_parent start_bb in

      let then_bb = L.append_block context "then" the_function in
      L.position_at_end then_bb builder;
      let then_val = codegen_sexpr cons map builder in

      (* Creating a new then bb allows if_then_else 
       * expressions to be nested recursively *)
      let new_then_bb = L.insertion_block builder in

      let else_bb = L.append_block context "else" the_function in
      L.position_at_end else_bb builder;
      let else_val = codegen_sexpr alt map builder in

      (* Creating a new else bb allows if_then_else 
       * expressions to be nested recursively *)
      let new_else_bb = L.insertion_block builder in

      (* Create merge basic block to wire everything up *)
      let merge_bb = L.append_block context "ifcont" the_function in
      L.position_at_end merge_bb builder;

      let incoming = [(then_val, new_then_bb); (else_val, new_else_bb)] in
      let phi = L.build_phi incoming "iftmp" builder in

      L.position_at_end start_bb builder;
      ignore (L.build_cond_br cond then_bb else_bb builder);

      L.position_at_end new_then_bb builder; ignore (L.build_br merge_bb builder);
      L.position_at_end new_else_bb builder; ignore (L.build_br merge_bb builder);

      (* Finally, set the builder to the end of the merge block. *)
      L.position_at_end merge_bb builder;

      phi in

  let fn_call fn params builder = 
    let fname = match fn with (_, SId(s)) -> s 
                | _ -> raise (Failure "Internal error - non-id in SApp")
    in let callee = match L.lookup_function fname the_module with
                | Some callee -> callee
                | None -> raise (Failure "Internal error - undefined function")
    in 
    L.build_call callee 
      (List.map (fun expr -> codegen_sexpr expr map builder) params 
                |> Array.of_list) 
      (fname ^ "_call") builder
    in

  let handle_id s = match L.lookup_function s the_module with
              None -> L.build_load (lookup s map) s builder
            | Some f -> print_endline "handle_id failure"; 
              L.build_call f [||] s builder in

  match typ with
  | SNat ->
    begin
      match detail with
      | SLiteral(i) -> L.const_int nat_t i
      | SAop(sexpr1, aop, sexpr2) ->
        let lhs = codegen_sexpr sexpr1 map builder in
        let rhs = codegen_sexpr sexpr2 map builder in
        begin
          match aop with
          | A.Add -> L.build_add lhs rhs "addnattmp" builder
          | A.Sub -> L.build_sub lhs rhs "subnattmp" builder
          | A.Mult -> L.build_mul lhs rhs "mulnattmp" builder
          | A.Div -> L.build_udiv lhs rhs "divnattmp" builder
          | A.Mod -> L.build_urem lhs rhs "modnattmp" builder
          | A.Expt -> 
            let ipow_t = L.function_type nat_t [| nat_t; nat_t |] in
            let ipow_func = L.declare_function "ipow" ipow_t the_module in
            L.build_call ipow_func [| lhs; rhs |] "ipow" builder
        end
      | SId(s) -> handle_id s
      | SApp(fn, params) -> fn_call fn params builder
      | SCondExpr(pred, cons, alt) -> cond_expr pred cons alt
      | _ -> raise (Failure "Internal error: semant should have rejected this")
    end
  | SBool ->
    begin
      match detail with
      | SBoolLit(b) -> L.const_int bool_t (if b then 1 else 0)
      | SId(s) -> handle_id s
      | SBoolop(sexpr1, bop, sexpr2) ->
        let lhs = codegen_sexpr sexpr1 map builder in
        let rhs = codegen_sexpr sexpr2 map builder in
        begin
          match bop with
          | A.And -> L.build_and lhs rhs "andtmp" builder
          | A.Or  -> L.build_or  lhs rhs "ortmp"  builder
        end
      | SRop(sexpr1, rop, sexpr2) ->
        let lhs = codegen_sexpr sexpr1 map builder in
        let rhs = codegen_sexpr sexpr2 map builder in
        begin
          match rop with
          | A.Eq  -> L.build_icmp L.Icmp.Eq  lhs rhs "eqtemp"  builder
          | A.Neq -> L.build_icmp L.Icmp.Ne  lhs rhs "neqtemp" builder
          | A.LT  -> L.build_icmp L.Icmp.Ult lhs rhs "lttemp"  builder
          | A.Leq -> L.build_icmp L.Icmp.Ule lhs rhs "leqtemp" builder
          | A.GT  -> L.build_icmp L.Icmp.Ugt lhs rhs "gttemp"  builder
          | A.Geq -> L.build_icmp L.Icmp.Uge lhs rhs "geqtemp" builder
        end
      | SApp(fn, params) -> fn_call fn params builder
      | SCondExpr(pred, cons, alt) -> cond_expr pred cons alt
      | _ -> raise (Failure "Internal error: semant should have blocked this")
    end
  (* Tensor of empty shape corresponds to single floating point number *)
  | STensor([]) -> 
    begin
      match detail with  
      | SFliteral(s) -> L.const_float_of_string float_t s
      | SUnop(A.Neg, sexpr) -> 
        L.build_fneg (codegen_sexpr sexpr map builder) "negfloattmp" builder
      | SId(s) -> handle_id s
      | SAop(sexpr1, aop, sexpr2) ->
        let lhs = codegen_sexpr sexpr1 map builder in
        let rhs = codegen_sexpr sexpr2 map builder in
        begin
          match aop with
          | A.Add -> L.build_fadd lhs rhs "addfloattmp" builder
          | A.Sub -> L.build_fsub lhs rhs "subfloattmp" builder
          | A.Mult -> L.build_fmul lhs rhs "mulfloattmp" builder
          | A.Div -> L.build_fdiv lhs rhs "divfloattmp" builder
          | A.Mod -> L.build_frem lhs rhs "modfloattmp" builder
          | A.Expt -> 
            let pow_t = L.function_type float_t [| float_t; float_t |] in
            let pow_func = L.declare_function "pow" pow_t the_module in
            L.build_call pow_func [| lhs; rhs |] "pow" builder
        end
      | SCondExpr(pred, cons, alt) -> cond_expr pred cons alt
      | SRop(sexpr1, rop, sexpr2) ->
        let lhs = codegen_sexpr sexpr1 map builder in
        let rhs = codegen_sexpr sexpr2 map builder in
        begin
          match rop with
          | A.Eq  -> L.build_fcmp L.Fcmp.Oeq rhs rhs "feqtemp"  builder
          | A.Neq -> L.build_fcmp L.Fcmp.One lhs rhs "fneqtemp" builder
          | A.LT  -> L.build_fcmp L.Fcmp.Olt lhs rhs "flttemp"  builder
          | A.Leq -> L.build_fcmp L.Fcmp.Ole lhs rhs "fleqtemp" builder
          | A.GT  -> L.build_fcmp L.Fcmp.Ogt lhs rhs "fgttemp"  builder
          | A.Geq -> L.build_fcmp L.Fcmp.Oge lhs rhs "fgeqtemp" builder
        end
      | SApp((_,SId("cast")), [param]) -> 
        L.build_uitofp (codegen_sexpr param map builder) 
          float_t "casted" builder
      | SApp(fn, params) -> fn_call fn params builder
      | _ -> raise (Failure "Internal error: semant failed")
    end
  | STensor(_shape) ->
    begin
      match detail with
      | STLit(contents, literal_shape) -> 
        let tsize = List.fold_left (fun acc elt -> acc * elt) 1 literal_shape in

        let trank = List.length literal_shape in

        let tshape_ptr = L.build_alloca (L.array_type nat_t trank) 
            "tshape_ptr" builder in
        let tshape_contents = 
          List.map (L.const_int nat_t) literal_shape |>
          Array.of_list |>
          L.const_array (L.array_type nat_t trank) in
        let _ = L.build_store tshape_contents tshape_ptr builder in

        let tcontents_ptr = L.build_alloca (L.array_type float_t tsize) 
            "tcontents_ptr" builder in
        let tcontents = 
          List.map (L.const_float_of_string float_t) contents |>
          Array.of_list |>
          L.const_array (L.array_type float_t tsize) in
        let _ = L.build_store tcontents tcontents_ptr builder in

        let tshape_ptr' = L.build_bitcast tshape_ptr (L.pointer_type nat_t)
            "bitcast_shape" builder in
        let tcontents_ptr' = L.build_bitcast tcontents_ptr (L.pointer_type float_t) 
            "bitcast_contents" builder in

        let the_ptr = 
          L.build_call talloc_func 
            [| L.const_int nat_t trank; 
               tshape_ptr';
               tcontents_ptr'|]
            "tensor_ptr" builder in the_ptr
      | SId(s) -> handle_id s
      | _ -> raise (Failure "WIP")
    end
  | _ -> raise (Failure "Not yet implemented")

(* Use in declaring global vars *)
let handle_const typ = match typ with
      SNat -> L.const_int nat_t 0
    | SBool -> L.const_int bool_t 0
    | STensor([]) -> L.const_float float_t 0.
    | STensor(_) -> L.const_pointer_null (L.pointer_type tensor_t)
    | _ -> raise (Failure "declare globals on unit type only")

(* Given a global var, declare it in the_module, and return a new map.
 * This function has the side effect of mutating the module *)
let declare_global env sfunc = 
  StringMap.add sfunc.sfname 
    (L.define_global sfunc.sfname (handle_const sfunc.stype) the_module) env

(* Given an sfunc, declare it in the_module, and return a new map.
 * This function has the side effect of mutating the module *)
let declare_fn env sfunc = 
  let the_typ = ltype_of_styp sfunc.stype in
  let the_function = L.define_function (sfunc.sfname) the_typ the_module in
  StringMap.add sfunc.sfname the_function env

(* Declare globals and functions for sfuncs *)
let codegen_proto env sfunc = 
    match List.length sfunc.sfparams with
      0 -> declare_global env sfunc 
    | _ -> declare_fn env sfunc

(* Codegen for globals *)
let codegen_global env sfunc builder = 
  ignore @@ L.build_store (codegen_sexpr sfunc.sfexpr env builder)
    (lookup sfunc.sfname env) builder;
  env

(* Codegen for function body *)
let codegen_fn_body env sfunc = 
    let the_function = match L.lookup_function sfunc.sfname the_module with
        | Some f -> f
        | None -> raise (Failure "internal error - undefined function")
        in
    (* let bb = L.append_block context (sfunc.sfname ^ "_entry") the_function in *)
    let fn_builder = L.builder_at_end context (L.entry_block the_function) in
    (* L.position_at_end bb fn_builder; *)

    (* Allocate function parameters:
        * Returns a new env *)
    let alloc_param env (typ, name) llval = 
        L.set_value_name name llval;
        let alloca = L.build_alloca (ltype_of_styp typ) name fn_builder in
        ignore @@ L.build_store llval alloca fn_builder;
        StringMap.add name alloca env in

    let env' = List.fold_left2 alloc_param 
        env sfunc.sfparams (L.params the_function |> Array.to_list) in

    let ret_val = codegen_sexpr sfunc.sfexpr env' fn_builder in
    let _ = L.build_ret ret_val fn_builder in 
    (* Return the new environment *)
    Llvm_analysis.assert_valid_function the_function;

    env'

(* Codegen on globals and functions *)
let codegen_body builder env sfunc = 
    match List.length sfunc.sfparams with
        0 -> codegen_global env sfunc builder
      | _ -> codegen_fn_body env sfunc

let translate sprogram =
  let sprogram_nodes = List.map Topsort.sfunc_to_node (snd sprogram) in
  let sprogram = (fst sprogram, (Topsort.topsort sprogram_nodes [])) in
  let main_ty = L.function_type (nat_t) [||] in
  let main = L.define_function "main" main_ty the_module in
  let builder = L.builder_at_end context (L.entry_block main) in

  (* Declare all defined functions *)
  let env = List.fold_left codegen_proto StringMap.empty (snd sprogram) in
  (* Build their bodies *)
  let env = List.fold_left (codegen_body builder) env (snd sprogram) in

  let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
  let bool_format_str = L.build_global_stringptr "%s\n" "fmt" builder in
  let float_format_str = L.build_global_stringptr "%g\n" "fmt" builder in
  let true_str = L.build_global_stringptr "True" "true_str" builder in
  let false_str = L.build_global_stringptr "False" "false_str" builder in

  let the_expression = codegen_sexpr (fst sprogram) env builder
  in ignore @@ (match fst (fst sprogram) with 
    | SNat -> L.build_call printf_func [| int_format_str ; the_expression |]
                 "printf" builder
    | SBool -> L.build_call printf_func [| bool_format_str ; 
            if the_expression = L.const_int bool_t 0 then false_str else true_str |]
                 "printf" builder
    | STensor([]) -> L.build_call printf_func 
                                [| float_format_str ; the_expression |]
                 "printf" builder
    | STensor(_) -> 
        let _ = L.build_call print_tensor_func [| the_expression |] 
            "print_tensor" builder in
        L.build_call tdelete_func [| the_expression |] "free_tensor" builder
    | SArrow(_,_) -> raise (Failure "Internal error: semant failed")
    );
    ignore @@ L.build_ret (L.const_int nat_t 0) builder;

  the_module

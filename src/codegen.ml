module L = Llvm
module A = Ast
open Sast
open Cast
open Semant

module StringMap = Map.Make(String)
let context = L.global_context ()
let the_module = L.create_module context "TensorFlock"

(* Declare the LLVM versions of all built in types *)
let nat_t = L.i32_type context
let bool_t = L.i1_type context
let i8_t = L.i8_type context
let float_t = L.double_type context

let rec ltype_of_ctyp = function 
    CNat -> nat_t
  | CBool -> bool_t
  | CDouble -> float_t
  | CTensor(size, _) -> L.vector_type float_t size

let printf_t = L.var_arg_function_type nat_t [| L.pointer_type i8_t |]
let printf_func = L.declare_function "printf" printf_t the_module

let print_tensor_t = L.var_arg_function_type nat_t [| L.pointer_type float_t; nat_t ; nat_t|]
let print_tensor_func = L.declare_function "print_tensor" print_tensor_t the_module

let dump_env map = StringMap.iter (fun name _value -> print_endline
    (name ^ " bound to " ^
    (L.string_of_llvalue _value)
    )) map

(* If this throws an error, then something is actually problematic and we have
 * a real bug. *)
let lookup name map = match StringMap.find_opt name map with
  | Some llval -> llval
  | None -> dump_env map;
            failwith @@ "Couldn't find " ^ name ^ " in the environment"

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
    let fname = match fn with (_, CId(s)) -> s 
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
  | CNat ->
    begin
      match detail with
      | CLiteral(i) -> L.const_int nat_t i
      | CAop(sexpr1, aop, sexpr2) ->
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
      | CId(s) -> handle_id s
      | CApp(fn, params) -> fn_call fn params builder
      | CCondExpr(pred, cons, alt) -> cond_expr pred cons alt
      | _ -> raise (Failure "Internal error: semant should have rejected this")
    end
  | CBool ->
    begin
      match detail with
      | CBoollit(b) -> L.const_int bool_t (if b then 1 else 0)
      | CId(s) -> handle_id s
      | CBoolop(sexpr1, bop, sexpr2) ->
        let lhs = codegen_sexpr sexpr1 map builder in
        let rhs = codegen_sexpr sexpr2 map builder in
        begin
          match bop with
          | A.And -> L.build_and lhs rhs "andtmp" builder
          | A.Or  -> L.build_or  lhs rhs "ortmp"  builder
        end
      | CRop(sexpr1, rop, sexpr2) ->
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
      | CApp(fn, params) -> fn_call fn params builder
      | CCondExpr(pred, cons, alt) -> cond_expr pred cons alt
      | _ -> raise (Failure "Internal error: semant should have blocked this")
    end
  (* Tensor of empty shape corresponds to single floating point number *)
  | CDouble -> 
    begin
      match detail with  
      | CFliteral(s) -> L.const_float_of_string float_t s
      | CUnop(A.Neg, sexpr) -> 
        L.build_fneg (codegen_sexpr sexpr map builder) "negfloattmp" builder
      | CId(s) -> handle_id s
      | CAop(sexpr1, aop, sexpr2) ->
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
      | CCondExpr(pred, cons, alt) -> cond_expr pred cons alt
      | CRop(sexpr1, rop, sexpr2) ->
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
      | CApp((_,CId("cast")), [param]) -> 
        L.build_uitofp (codegen_sexpr param map builder) 
          float_t "casted" builder
      | CApp(fn, params) -> fn_call fn params builder
      | CTensorIdx(e, idx) -> 
        let the_expr = codegen_sexpr e map builder in
        L.build_extractelement the_expr (L.const_int nat_t idx) "extract" builder
      | _ -> raise (Failure "Internal error: semant failed")
    end
  | CTensor(_size, _shape) -> 
          (* print_endline @@ "At cexpr node " ^ string_of_cexpr (typ, detail) *)
          (* ^ " whose type is " ^ string_of_ctyp typ; *)
    begin
      match detail with
      | CTlit(contents, _tsize) -> 
          L.const_vector @@ Array.map (L.const_float_of_string float_t) contents
      | CId(s) -> handle_id s
      | CApp(fn, params) -> fn_call fn params builder
      | CCondExpr(pred, cons, alt) -> cond_expr pred cons alt
      | _ -> failwith @@ "Handling expression: "  ^ string_of_cexpr (typ, detail)
    end

(* Use in declaring global vars *)
let handle_const assign = match assign.typ with
      CNat -> L.const_int nat_t 0
    | CBool -> L.const_int bool_t 0
    | CDouble -> L.const_float float_t 0.
    | CTensor(_size, _shape) -> L.const_all_ones (ltype_of_ctyp assign.typ)

(* Given a global var, declare it in the_module, and return a new map.
 * This function has the side effect of mutating the module *)
let declare_global env assign = 
    (* Assignments are either global variables or assignments to a tensor entry 
     * Process global var declarations here and defer specific index assignment
     * to later in translate *)
    match assign.index with
      | None -> StringMap.add assign.name
            (L.define_global assign.name (handle_const assign) the_module)
            env
      | Some _ -> env

(* Given an sfunc, declare it in the_module, and return a new map.
 * This function has the side effect of mutating the module *)
let declare_fn env cfunc = 
  let param_typs = 
        (List.map (fun (t,_) -> ltype_of_ctyp t) cfunc.params) |> Array.of_list in
  let the_typ = L.function_type (ltype_of_ctyp cfunc.ret_typ) param_typs in
  let the_function = L.define_function (cfunc.cname) the_typ the_module in
  StringMap.add cfunc.cname the_function env

(* Codegen for globals *)
let codegen_global env builder assign = 
  match assign.index with 
    | None -> ignore @@ L.build_store (codegen_sexpr assign.cexpr env builder)
              (lookup assign.name env) builder; env 
    | Some i ->
            (* print_endline @@ "Building assignment " ^ string_of_cexpr assign.cexpr ^ " : " ^ *)
            (* string_of_ctyp (fst assign.cexpr); *)
            
        let elt = codegen_sexpr assign.cexpr env builder in
        let vec_ptr = lookup assign.name env in
        let vec = L.build_load vec_ptr "" builder in
        let new_vec = 
            L.build_insertelement vec elt (L.const_int nat_t i) "" builder in
        ignore @@ 
        L.build_store new_vec vec_ptr builder; env

(* Codegen for function body *)
let codegen_fn_body env cfunc = 
    let the_function = match L.lookup_function cfunc.cname the_module with
        | Some f -> f
        | None -> raise (Failure "internal error - undefined function")
        in
    let fn_builder = L.builder_at_end context (L.entry_block the_function) in

    (* Allocate function parameters:
        * Returns a new env *)
    let alloc_param env (typ, name) llval = 
        L.set_value_name name llval;
        let alloca = L.build_alloca (ltype_of_ctyp typ) name fn_builder in
        ignore @@ L.build_store llval alloca fn_builder;
        StringMap.add name alloca env in

    let env' = 
      List.fold_left2 alloc_param 
        env cfunc.params (L.params the_function |> Array.to_list) in

    (* Allocate scope variables:
        * Returns a new env *) 
    let alloc_scope scope env = 
      let llvals = List.fold_right (fun assign acc ->
          match assign.index with
            | None -> handle_const assign :: acc
            | Some _ -> acc
      ) scope [] in
      List.fold_left2 (fun acc assign llval -> 
          match assign.index with
            | None -> alloc_param acc (assign.typ, assign.name) llval
            | Some _ -> acc)
        env (List.filter (fun assign -> assign.index = None) scope) llvals in
    let env'' = alloc_scope cfunc.locals env' in 

    (* codegen on variables in scope, adding their values to env'' *) 
    List.iter (fun assign -> 
        match assign.index with
          | None -> ignore @@ L.build_store (codegen_sexpr assign.cexpr env'' fn_builder) 
                    (lookup assign.name env'') fn_builder
          | Some i -> 
              let elt = codegen_sexpr assign.cexpr env'' fn_builder in
              let vec_ptr = lookup assign.name env'' in
              let vec = L.build_load vec_ptr "" fn_builder in
              let new_vec = 
                  L.build_insertelement vec elt (L.const_int nat_t i) "" fn_builder in
              ignore @@ 
              L.build_store new_vec vec_ptr fn_builder;
          ) cfunc.locals;

    let ret_val = codegen_sexpr cfunc.cfexpr env'' fn_builder in
    let _ = L.build_ret ret_val fn_builder in 
    (* Return the new environment *)
    Llvm_analysis.assert_valid_function the_function;

    env''

let translate (main_expr, assigns, cfuncs) ppm_flag =
  let main_ty = L.function_type (nat_t) [||] in
  let main = L.define_function "main" main_ty the_module in
  let builder = L.builder_at_end context (L.entry_block main) in

  (* Declare all defined variables *)
  let env = List.fold_left declare_global StringMap.empty assigns in
  (* Declare all defined functions *)
  let env = List.fold_left declare_fn env cfuncs in
  (* Build global variables *)
  let env = List.fold_left 
      (fun acc assign -> codegen_global acc builder assign) env assigns in
  (* Build function bodies *)
  let env = List.fold_left codegen_fn_body env cfuncs in

  let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
  let bool_format_str = L.build_global_stringptr "%s\n" "fmt" builder in
  let float_format_str = L.build_global_stringptr "%g\n" "fmt" builder in
  let true_str = L.build_global_stringptr "True" "true_str" builder in
  let false_str = L.build_global_stringptr "False" "false_str" builder in

  let the_expression = codegen_sexpr main_expr env builder
  in ignore @@ (match fst main_expr with 
    | CNat -> L.build_call printf_func [| int_format_str ; the_expression |]
                 "printf" builder
    | CBool -> L.build_call printf_func [| bool_format_str ; 
            if the_expression = L.const_int bool_t 0 then false_str else true_str |]
                 "printf" builder
    | CDouble -> L.build_call printf_func 
                                [| float_format_str ; the_expression |]
                 "printf" builder
    | CTensor(_size, shape) -> 
        let rank = L.const_int nat_t (List.length shape) in
        let alloca = L.build_alloca (ltype_of_ctyp (fst main_expr)) "" builder
        in
        let _ = L.build_store the_expression alloca builder in
        let ptr_of_vec = L.build_bitcast 
            alloca (L.pointer_type float_t) "" builder in
        let print_args = 
          [ptr_of_vec; rank; if ppm_flag then (L.const_int nat_t 1) else
             (L.const_int nat_t 0)] @ List.map (L.const_int nat_t) shape in
        L.build_call print_tensor_func (Array.of_list print_args)
            "print_tensor" builder
    );
    ignore @@ L.build_ret (L.const_int nat_t 0) builder;

  the_module

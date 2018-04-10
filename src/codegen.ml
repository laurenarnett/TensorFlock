module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)
let context = L.global_context ()
let the_module = L.create_module context "TensorFlock"

(* Declare the LLVM versions of all built in types *)
let nat_t = L.i32_type context
let bool_t = L.i1_type context
let i8_t = L.i8_type context
let float_t = L.double_type context



let printf_t = L.var_arg_function_type nat_t [| L.pointer_type i8_t |]
let printf_func = L.declare_function "printf" printf_t the_module

let tensor_t = L.struct_type context [|
    nat_t; (* size: number of doubles the tensor holds *)
    nat_t; (* rank *)
    L.pointer_type nat_t; (* shape *)
    L.pointer_type nat_t; (* number of references *)
    L.pointer_type float_t; (* contents *)
  |]

let ltype_of_unit = function 
    A.Nat -> nat_t
  | A.Bool -> bool_t
  | A.Tensor([]) -> float_t
  | A.Tensor(_) -> tensor_t

let rec ltype_of_arrow = function
    A.Arrow(fn1, _) -> ltype_of_arrow fn1
  | A.Unit(u) -> ltype_of_unit u

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

let lookup name global_vars = try StringMap.find name global_vars
                    with Not_found -> raise (Failure "WIP")

let rec codegen_sexpr (typ, detail) globals builder = 
  let cond_expr pred cons alt = 
      (* Wholesale copying of the Kaleidescope tutorial's conditional
       * expression codegen *)
      let cond = codegen_sexpr pred globals builder in
      (* Grab the first block so that we might later add the conditional branch
       * to it at the end of the function. *)
      let start_bb = L.insertion_block builder in
      let the_function = L.block_parent start_bb in

      let then_bb = L.append_block context "then" the_function in
      L.position_at_end then_bb builder;
      let then_val = codegen_sexpr cons globals builder in

      (* Creating a new then bb allows if_then_else 
       * expressions to be nested recursively *)
      let new_then_bb = L.insertion_block builder in

      let else_bb = L.append_block context "else" the_function in
      L.position_at_end else_bb builder;
      let else_val = codegen_sexpr alt globals builder in

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


  match typ with
  | A.Unit(A.Nat) ->
    begin
      match detail with
      | SLiteral(i) -> L.const_int nat_t i
      | SAop(sexpr1, aop, sexpr2) ->
        let lhs = codegen_sexpr sexpr1 globals builder in
        let rhs = codegen_sexpr sexpr2 globals builder in
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
      | SId(s) -> L.build_load (lookup s globals) s builder
      | SApp(fn, formals) -> 
        begin
          match fn with
          
          | (_, SId(s)) -> 
            let ll_func = lookup s globals in
            let the_exprs = List.map (fun expr -> 
                                codegen_sexpr expr globals builder) formals in
              L.build_call ll_func (Array.of_list the_exprs) "calltmp" builder
          | _ -> raise (Failure "Semant should have caught this")
        end
      | SCondExpr(pred, cons, alt) -> cond_expr pred cons alt
      | _ -> raise (Failure "Internal error: semant should have rejected this")
    end
  | A.Unit(A.Bool) ->
    begin
      match detail with
      | SBoolLit(b) -> L.const_int bool_t (if b then 1 else 0)
      | SId(s) -> L.build_load (lookup s globals) s builder
      | SBoolop(sexpr1, bop, sexpr2) ->
        let lhs = codegen_sexpr sexpr1 globals builder in
        let rhs = codegen_sexpr sexpr2 globals builder in
        begin
          match bop with
          | A.And -> L.build_and lhs rhs "andtmp" builder
          | A.Or  -> L.build_or  lhs rhs "ortmp"  builder
        end
      | SRop(sexpr1, rop, sexpr2) ->
        let lhs = codegen_sexpr sexpr1 globals builder in
        let rhs = codegen_sexpr sexpr2 globals builder in
        begin
          match rop with
          | A.Eq  -> L.build_icmp L.Icmp.Eq  lhs rhs "eqtemp"  builder
          | A.Neq -> L.build_icmp L.Icmp.Ne  lhs rhs "neqtemp" builder
          | A.LT  -> L.build_icmp L.Icmp.Ult lhs rhs "lttemp"  builder
          | A.Leq -> L.build_icmp L.Icmp.Ule lhs rhs "leqtemp" builder
          | A.GT  -> L.build_icmp L.Icmp.Ugt lhs rhs "gttemp"  builder
          | A.Geq -> L.build_icmp L.Icmp.Uge lhs rhs "geqtemp" builder
        end
      | SApp(_,_) -> raise (Failure "WIP")
      | SCondExpr(pred, cons, alt) -> cond_expr pred cons alt
      | _ -> raise (Failure "Internal error: semant should have blocked this")
    end
  (* Tensor of empty shape corresponds to single floating point number *)
  | A.Unit(A.Tensor([])) -> 
    begin
      match detail with  
      | SFliteral(s) -> L.const_float_of_string float_t s
      | SUnop(A.Neg, sexpr) -> 
        L.build_fneg (codegen_sexpr sexpr globals builder) "negfloattmp" builder
      | SId(s) -> L.build_load (lookup s globals) s builder
      | SAop(sexpr1, aop, sexpr2) ->
        let lhs = codegen_sexpr sexpr1 globals builder in
        let rhs = codegen_sexpr sexpr2 globals builder in
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
        let lhs = codegen_sexpr sexpr1 globals builder in
        let rhs = codegen_sexpr sexpr2 globals builder in
        begin
          match rop with
          | A.Eq  -> L.build_fcmp L.Fcmp.Oeq rhs rhs "feqtemp"  builder
          | A.Neq -> L.build_fcmp L.Fcmp.One lhs rhs "fneqtemp" builder
          | A.LT  -> L.build_fcmp L.Fcmp.Olt lhs rhs "flttemp"  builder
          | A.Leq -> L.build_fcmp L.Fcmp.Ole lhs rhs "fleqtemp" builder
          | A.GT  -> L.build_fcmp L.Fcmp.Ogt lhs rhs "fgttemp"  builder
          | A.Geq -> L.build_fcmp L.Fcmp.Oge lhs rhs "fgeqtemp" builder
        end
      | SApp(_,_) -> raise (Failure "Functions not yet implemented")
      | _ -> raise (Failure "Internal error: semant failed")
    end
  | A.Unit(A.Tensor(_shape)) ->
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
      | SId(s) -> L.build_load (lookup s globals) s builder
      | _ -> raise (Failure "WIP")
    end
  | _ -> raise (Failure "Not yet implemented")

let translate sprogram =

  let main_ty = L.function_type (nat_t) [||] in
  let main = L.define_function "main" main_ty the_module in
  let builder = L.builder_at_end context (L.entry_block main) in
  let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
  let bool_format_str = L.build_global_stringptr "%s\n" "fmt" builder in
  let float_format_str = L.build_global_stringptr "%g\n" "fmt" builder in
  let true_str = L.build_global_stringptr "True" "true_str" builder in
  let false_str = L.build_global_stringptr "False" "false_str" builder in

  let handle_const typ = match typ with
      A.Nat -> L.const_int nat_t 0
    | A.Bool -> L.const_int bool_t 0
    | A.Tensor([]) -> L.const_float float_t 0.
    | A.Tensor(_) -> L.const_pointer_null (L.pointer_type tensor_t)
  in

  (* Declare global variables; save each value in a map*)
  let the_global_vars = 
    let global_var map (typ, name) = 
      let init = handle_const typ
    in StringMap.add name (L.define_global name init the_module) map
  in List.fold_left 
        begin
          fun map fn ->
            match fn.stype with
            | A.Unit(t) -> global_var map (t, fn.sfname)
            | A.Arrow(_, _) -> map
        end
        StringMap.empty (snd sprogram) in

  (* Add function decls to StringMap *)
  let the_function_decls = 
    let function_decl map fdecl = 
      let name = fdecl.sfname
      and type_signature = 
        Array.of_list (List.map ltype_of_unit (Semant.list_of_type fdecl.stype)) in
      let return_type = type_signature.(Array.length type_signature - 1) in
      let formal_types = Array.sub type_signature 0 (Array.length
          type_signature - 1) in
      let ftype = L.function_type return_type formal_types in
      StringMap.add name (L.define_function name ftype the_module) map
   in List.fold_left
       begin
         fun map fn ->
           match fn.stype with 
           | A.Unit(_) -> map
           | A.Arrow(_, _) -> function_decl map fn
        end
        the_global_vars (snd sprogram) in

  (* Build the function body *)  
  let build_function_body sfunc = 
    let the_function = StringMap.find sfunc.sfname the_function_decls in
    let the_function_bb = L.append_block context sfunc.sfname the_function in
    let fn_builder = L.builder_at_end context the_function_bb in 
    let the_expr = codegen_sexpr sfunc.sfexpr the_global_vars fn_builder in
    let _fn_ret = L.build_ret the_expr fn_builder in
    let _ = L.position_at_end the_function_bb builder in

  let _codegen_globals = 
    List.map (fun sfunc -> L.build_store 
                  (codegen_sexpr sfunc.sfexpr the_global_vars builder)
                  (lookup sfunc.sfname the_global_vars) builder) 
  (snd sprogram) in
  let main_expression = codegen_sexpr (fst sprogram) the_global_vars builder
  in ignore @@ (match fst (fst sprogram) with 
    | A.Unit(A.Nat) -> L.build_call printf_func [| int_format_str
                                                 ; main_expression |]
                 "printf" builder
    | A.Unit(A.Bool) -> L.build_call printf_func [| bool_format_str ; 
            if main_expression = L.const_int bool_t 0 then false_str else true_str |]
                 "printf" builder
    | A.Unit(A.Tensor([])) -> L.build_call printf_func 
                                [| float_format_str ; main_expression |]
                 "printf" builder
    | A.Unit(A.Tensor(_)) -> 
        let _ = L.build_call print_tensor_func [| main_expression |] 
            "print_tensor" builder in
        L.build_call tdelete_func [| main_expression |] "free_tensor" builder
    | A.Arrow(_,_) -> raise (Failure "Internal error: semant failed")
    );
  ignore @@ L.build_ret (L.const_int nat_t 0) builder in

  List.iter build_function_body (snd sprogram);
  the_module

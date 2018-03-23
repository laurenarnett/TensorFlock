module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)
let context = L.global_context ()
let the_module = L.create_module context "TensorFlock"
let nat_t = L.i32_type context
let bool_t = L.i1_type context
let i8_t = L.i8_type context
let ltype_of_typ = function
    A.Unit(A.Nat) -> nat_t
  | A.Unit(A.Bool) -> bool_t
  | t -> raise (Failure ("Type " ^ A.string_of_typ t ^ " not implemented."))

let rec codegen_sexpr (typ, detail) builder = match typ with
  | A.Unit(A.Nat) ->
    begin
      match detail with
      | SLiteral(i) -> L.const_int nat_t i
      | SAop(sexpr1, aop, sexpr2) ->
        let lhs = codegen_sexpr sexpr1 builder in
        let rhs = codegen_sexpr sexpr2 builder in
        begin
          match aop with
          | A.Add -> L.build_add lhs rhs "addnattmp" builder
          | A.Sub -> L.build_sub lhs rhs "subnattmp" builder
          | A.Mult -> L.build_mul lhs rhs "mulnattmp" builder
          | A.Div -> L.build_udiv lhs rhs "divnattmp" builder
          | A.Mod -> L.build_urem lhs rhs "modnattmp" builder
          | A.Expt -> raise (Failure "WIP")
        end
      | SId(_) -> raise (Failure "WIP")
      | SApp(_,_) -> raise (Failure "Not yet implemented")
      | SCondExpr(_,_,_) -> raise (Failure "WIP")
      | _ -> raise (Failure "Internal error: semant should have rejected this")
    end
  | A.Unit(A.Bool) ->
    begin
      match detail with
      | SBoolLit(b) -> L.const_int bool_t (if b then 1 else 0)
      | SId(_) -> raise (Failure "WIP")
      | SBoolop(sexpr1, bop, sexpr2) ->
        let lhs = codegen_sexpr sexpr1 builder in
        let rhs = codegen_sexpr sexpr2 builder in
        begin
          match bop with
          | A.And -> L.build_and lhs rhs "andtmp" builder
          | A.Or  -> L.build_or  lhs rhs "ortmp"  builder
        end
      | SRop(sexpr1, rop, sexpr2) ->
        let lhs = codegen_sexpr sexpr1 builder in
        let rhs = codegen_sexpr sexpr2 builder in
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
      | SCondExpr(_,_,_) -> raise (Failure "WIP")
      | _ -> raise (Failure "Internal error: semant should have blocked this")
    end
  | _ -> raise (Failure "Not yet implemented")

let translate sprogram =

  let printf_t : L.lltype =
    L.var_arg_function_type nat_t [| L.pointer_type i8_t |] in

  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t the_module in

  let to_imp str = raise (Failure ("Not yet implemented: " ^ str)) in

  let main_ty = L.function_type (nat_t) [||] in
  let main = L.define_function "main" main_ty the_module in
  let builder = L.builder_at_end context (L.entry_block main) in
  let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
  let bool_format_str = L.build_global_stringptr "%s\n" "fmt" builder in
  let true_str = L.build_global_stringptr "True" "true_str" builder in
  let false_str = L.build_global_stringptr "False" "false_str" builder in

  let the_expression = codegen_sexpr (fst sprogram) builder
  (* in let the_main_global = L.define_global "main" the_expression the_module
   * *)
  in ignore @@ (match fst (fst sprogram) with 
    | A.Unit(A.Nat) -> L.build_call printf_func [| int_format_str ; the_expression |]
                 "printf" builder
    | A.Unit(A.Bool) -> L.build_call printf_func [| bool_format_str ; 
            if the_expression = L.const_int bool_t 0 then false_str else true_str |]
                 "printf" builder
    | _ -> to_imp "No tensors yet"
    );
    ignore @@ L.build_ret (L.const_int nat_t 0) builder;

  the_module

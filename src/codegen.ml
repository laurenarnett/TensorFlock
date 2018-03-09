module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

let tranlate (expr, functions) = 
    let context = L.global_context () in
    let i32_t = L.i32_type context
    and i8_t = L.i8_type context
    and the_module = L.create_module context "TensorFlock" in

    let ltype_of_typ = function
        A.Nat -> i32_t
      | t -> raise (Failure ("Type " ^ A.string_of_typ t ^ " not implemented."))
    in

    let printf_t : L.lltype =
        L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
    let printf_func : L.llvalue =
        L.declare_function "printf" printf_t the_module in 

    let to_imp str = raise (Failure ("Not yet implemented: " ^ str)) in

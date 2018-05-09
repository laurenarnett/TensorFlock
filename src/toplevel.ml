(* Top-level of the TensorFlock compiler: scan & parse the input *)

type action = Ast | Sast | Lift | Sort | Cast | LLVM_IR | Compile

let () =
  let action = ref Compile in
  let set_action a () = action := a in
  let speclist = [
    ("-a", Arg.Unit (set_action Ast),  "Print the AST");
    ("-s", Arg.Unit (set_action Sast), "Run the semantic checker");
    ("-lift", Arg.Unit (set_action Lift), "Print lambda lifted code");
    ("-sort", Arg.Unit (set_action Sort), "Sort the variables");
    ("-cast", Arg.Unit (set_action Cast), "Print the CAST");
    ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
    ("-c", Arg.Unit (set_action Compile),
      "Check and print the generated LLVM IR (default)");
  ] in
  let usage_msg = "usage: ./toplevel.native [-a|-s|-l|-c] [file.tf]" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;

  let lexbuf = Lexing.from_channel !channel in
  let ast = Parser.program Scanner.token lexbuf in
  match !action with
     Ast -> print_endline (Ast.string_of_program ast)
   (* Wire together the lambda lifter and variable sorter here *)
   | _ -> let sast = Semant.check ast in
  match !action with
     Ast -> ()
   | Sast -> print_endline (Sast.string_of_sprogram sast)
   | _ -> let lifted_sast = Lift.lift_sprogram sast in 
  match !action with
   | Ast | Sast -> ()
   | Lift -> print_endline (Sast.string_of_sprogram lifted_sast)
   | _ -> let sorted_sast = Topsort.make_topsort lifted_sast in
  match !action with
   | Ast | Sast | Lift -> ()
   | Sort -> print_endline (Sast.string_of_sprogram sorted_sast)
   | _ -> let cast = Cast.cprogram_of_sprogram sorted_sast in
  match !action with
   | Ast | Sast | Lift | Sort -> ()
   | Cast -> print_endline (Cast.string_of_cprogram cast)
   | LLVM_IR -> print_endline 
                (Llvm.string_of_llmodule (Codegen.translate cast))
   | Compile -> let mdl = Codegen.translate cast in
   Llvm_analysis.assert_valid_module mdl;
   ignore @@ Llvm_bitwriter.write_bitcode_file mdl "output.ll"

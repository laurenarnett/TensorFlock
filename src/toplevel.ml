(* Top-level of the TensorFlock compiler: scan & parse the input *)

type action = Ast | Sast | LLVM_IR | Compile

let () =
  let action = ref Compile in
  let set_action a () = action := a in
  let speclist = [
    ("-a", Arg.Unit (set_action Ast),  "Print the AST");
    ("-s", Arg.Unit (set_action Sast), "Run the semantic checker");
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
     Ast -> print_string (Ast.string_of_program ast)
   | Sast -> let symbol_table = Semant.build_table Semant.empty_table (snd ast) in
               print_string ("WIP\n" ^ (Semant.string_of_table symbol_table))
               (* (Sast.string_of_sprogram (Semant.check ast)) *)
   | LLVM_IR -> print_string ("Not implemented")  
   | Compile -> print_string ("Not implemented")  

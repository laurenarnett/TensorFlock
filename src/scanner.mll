(* Ocamllex scanner for TensorFlock *)

{ open Parser }

let digit = ['0' - '9']
let digits = digit+
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* ('\'')* 
let whitespace = [' ' '\t' '\r' '\n']

rule token = parse
   whitespace { token lexbuf } (* Whitespace *)
| "/*"     { block_comment lexbuf }     (* Block Comments *)
| "//"     { line_comment lexbuf }      (* Line  Comments *)
| ';'      { SEMI }
| ':'      { COLON }
| ','      { COMMA }
| "->"     { ARROW }
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LBRACK }
| ']'      { RBRACK }
| ">="     { GEQ }
| "<="     { LEQ }
| '<'      { LANGLE }
| '>'      { RANGLE }
| "=="     { EQ }
| '='      { DEFINE }
| "!="     { NEQ }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '%'      { MOD }
| '^'      { EXPT }
| "&&"     { AND }
| "||"     { OR }
| "if"     { IF }
| "then"   { THEN }
| "else"   { ELSE }
| "Nat"    { NAT }
| "Bool"   { BOOL }
| "True"   { BLIT(true)  }
| "False"  { BLIT(false) }
| 'T'      { TENSOR }
| "main"   { MAIN }
| '#' (.+ as filepath) ';' { INCLUDE(filepath) } 
| digits as lxm { LITERAL(int_of_string lxm) }
| '-'? digits '.'  digit* ( ['e' 'E'] ['+' '-']? digits )? as lxm { FLIT(lxm) }
| id   as lxm { ID(lxm) }
(* Require that tensor indexing have no whitespace after an identifier *)
| (id as lxm) '[' { TIDX(lxm) } 
(* Fnames and params can have indices attached to them *)
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }


and block_comment = parse
  "*/" { token lexbuf }
| _    { block_comment lexbuf }

and line_comment = parse
  "\n" { token lexbuf }
| _    { line_comment lexbuf }


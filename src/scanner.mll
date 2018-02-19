(* Ocamllex scanner for TensorFlock *)

{ open Parser }

let digit = ['0' - '9']
let digits = digit+

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
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
(* unclear if <= needs to be before < in order
 * for < to not succeed by default every time *)
| ">="     { GEQ }
| "<="     { LEQ }
| '<'      { LANGLE }
| '>'      { RANGLE }
| "=="     { EQ }
| "!"      { NOT }
| '='      { DEFINE }
| "!="     { NEQ }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '%'      { MOD }
| '^'      { EXPT }
(* TODO: decide how to disambiguate tensor shape and GT/LT *)
(* | '<'      { LT } *)
(* | ">"      { GT } *)
| "&&"     { AND }
| "||"     { OR }
| "if"     { IF }
| "then"   { THEN }
| "else"   { ELSE }
| "Int"    { INT }
| "Bool"   { BOOL }
| "Double" { DOUBLE }
| "True"   { BLIT(true)  }
| "False"  { BLIT(false) }
| 'T'      { TENSOR }
| digits as lxm { LITERAL(int_of_string lxm) }
| digits '.'  digit* ( ['e' 'E'] ['+' '-']? digits )? as lxm { FLIT(lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* ('\'')*    as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }


and block_comment = parse
  "*/" { token lexbuf }
| _    { block_comment lexbuf }

and line_comment = parse
  "\n" { token lexbuf }
| _    { line_comment lexbuf }


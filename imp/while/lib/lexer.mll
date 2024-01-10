{
  open Parser
}

let white = [' ' '\t' '\n']+
let var = ['a'-'z'](['A'-'Z''a'-'z']*['_']*['0'-'9']*)*
let num = ['0'-'9']|['1'-'9']['0'-'9']*

rule read = 
  parse 
  | white { read lexbuf }
  | "true" { TRUE }
  | "false" {FALSE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "not" { NOT }
  | "and" { AND }
  | "or" { OR }
  | "skip" { SKIP }
  | "while" { WHILE }
  | "do" { DO }
  | ";" { SM }
  | "+" { ADD }
  | "-" { SUB }
  | "*" { MUL }
  | "=" { EQ }
  | ":=" { ASSIGN }
  | "<=" { LEQ }
  | var { VAR(Lexing.lexeme lexbuf) }
  | num { CONST(Lexing.lexeme lexbuf) }
  | eof { EOF }
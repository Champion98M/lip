{
  open Token
}

let white = [' ' '\t']+
let letter = ['a'-'z' 'A'-'Z']
let chr = ['a'-'z' 'A'-'Z' '0'-'9']
let atok = ['A'-'Z'] chr*
let btok = ['a' 'e' 'i' 'o' 'u']+
let ctok = ['b'-'d' 'f'-'h' 'j'-'n' 'p'-'t' 'v'-'z' 'B'-'D' 'F'-'H' 'J'-'N' 'P'-'T' 'V'-'Z']*['a' 'e' 'i' 'o' 'u' 'A' 'E' 'I' 'O' 'U']?['b'-'d' 'f'-'h' 'j'-'n' 'p'-'t' 'v'-'z' 'B'-'D' 'F'-'H' 'J'-'N' 'P'-'T' 'V'-'Z']*
let dtok = ['-']['.']?['0'-'9']+|['-']?['0'-'9']+['.']['0'-'9']*
let etok = ['0']['x']['0'-'9' 'a'-'f' 'A'-'F']+|['0']['X']['0'-'9' 'a'-'f' 'A'-'F']+
let id = letter chr*
let num = ['0'-'9']|['1'-'9']['0'-'9']*

rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "=" { ASSIGN }
  | "+" { PLUS }
  | ";" { SEQ }  
  | atok { ATOK (Lexing.lexeme lexbuf)}
  | btok { BTOK (Lexing.lexeme lexbuf)}
  | ctok { CTOK (Lexing.lexeme lexbuf)}
  | dtok { DTOK (Lexing.lexeme lexbuf)}
  | etok { ETOK (Lexing.lexeme lexbuf)}
  | id { ID (Lexing.lexeme lexbuf) }
  | num { CONST (Lexing.lexeme lexbuf) }    
  | eof { EOF }

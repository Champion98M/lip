{ open Parser }

let white = [' ' '\t']+
let w = [' ' '\t']
let num = ['0'-'9']|['1'-'9']['0'-'9']*
let ext_num = w* num w* ([',']w*num)* w*
let range = num".."num
let value = range (([',']range)* ([',']ext_num)*)* | ext_num (([',']range)* ([',']ext_num)*)*

rule read_token = 
  parse
  | white { read_token lexbuf }
  | num { VALUES (Lexing.lexeme lexbuf) }
  | ext_num { VALUES (Lexing.lexeme lexbuf) }
  | range { let nums = List.filter (fun x -> x<>"") (String.split_on_char '.' (Lexing.lexeme lexbuf)) in (match nums with | [start; end_] -> if int_of_string start <= int_of_string end_ then VALUES(Lexing.lexeme lexbuf) else failwith "Invalid range" | _ -> failwith "Invalid Range")}
  | value { VALUES (Lexing.lexeme lexbuf) }
  | eof { EOF } 
  | "S" { SRV }
  | "B" { BRN }
  | "/" { SEP }
  | "E" { EXT }

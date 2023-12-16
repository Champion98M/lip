open Ast

(* parse : string -> ast *)

let parse (s : string) : ast =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read_token lexbuf in
  ast

type result = int option

let string_of_result = function
  | Some n -> string_of_int n
  | None -> "Error"
    
(* eval : ast -> result *)
    
let rec eval = function
    Const(n) ->  Some n
  | Add(e1,e2) -> 
      (match (eval e1, eval e2) with
        | (Some v1, Some v2) -> Some (v1+v2)
        | _ -> None)
  | Sub(e1,e2) -> 
      (match (eval e1, eval e2) with
        | (Some v1, Some v2) -> Some (v1-v2)
        | _ -> None)
  | Mul(e1,e2) -> 
      (match (eval e1, eval e2) with
        | (Some v1, Some v2) -> Some (v1*v2)
        | _ -> None)
  | Div(e1,e2) ->
      (match (eval e1, eval e2) with
        | (_, Some 0) -> None
        | (Some v1, Some v2) -> Some (v1/v2)
        | _ -> None)
  | Minus(e) ->
      (match eval e with
        | Some e1 -> Some (-e1)
        | _ -> None)
  | Hex(n) -> Some n



                    

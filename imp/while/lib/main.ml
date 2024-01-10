open Ast
open Types
open Prettyprint

let parse (s:string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in 
  ast
;;

let bool_of_expr = function
  | Bool(true) -> true
  | Bool(false) -> false
  | _ -> failwith "Wrong type: expected type was bool, but int was provided"
;;

let int_of_expr = function
 | Nat(n) -> n
 | _ -> failwith "Wrong type: expected type was int, but bool was provided"
;;

exception UnboundVar of string
let s0 x = raise (UnboundVar x)


let rec eval_expr s = function
  | True -> Bool true
  | False -> Bool false
  | Var x -> s x
  | Const n -> Nat n
  | Not e -> Bool(not (bool_of_expr (eval_expr s e)))
  | And(e1,e2) -> Bool(bool_of_expr(eval_expr s e1) && bool_of_expr(eval_expr s e2))
  | Or(e1,e2) -> Bool(bool_of_expr(eval_expr s e1) || bool_of_expr(eval_expr s e2))
  | Add(e1,e2) -> Nat(int_of_expr(eval_expr s e1) + int_of_expr(eval_expr s e2))
  | Sub(e1,e2) -> Nat(int_of_expr(eval_expr s e1) - int_of_expr(eval_expr s e2))
  | Mul(e1,e2) -> Nat(int_of_expr(eval_expr s e1) * int_of_expr(eval_expr s e2))
  | Eq(e1,e2) -> Bool(eval_expr s e1 = eval_expr s e2)
  | Leq(e1,e2) -> Bool(int_of_expr(eval_expr s e1) <= int_of_expr(eval_expr s e2))
;;

(*let rec trace1 = function
  | St _ -> raise NoRuleApplies
  | Cmd(cmd,s) -> (match cmd with
    | Skip -> St s
    | Assign(x,e) -> let v = eval_expr s e in let s' y = if y=x then v else s y in St s'
    | Seq(c1,c2) -> (match trace1(Cmd(c1,s)) with
                      | St s' -> trace1(Cmd(c2,s'))
                      | Cmd(c1',s') -> Cmd(Seq(c1',c2),s'))
    | If(e,c1,c2) -> if bool_of_expr(eval_expr s e) then trace1(Cmd(c1,s)) else trace1(Cmd(c2,s))
    | While(e,c) -> let rec loop cnf = (match cnf with 
                                          | St s' -> loop(Cmd(c,s'))
                                          | Cmd(c',s') -> if bool_of_expr (eval_expr s' e) then loop (trace1(Cmd(c',s'))) else St s') in loop (Cmd(c,s)))
;;*)



let rec trace1 = function
  | St _ -> raise NoRuleApplies
  | Cmd (cmd, env) -> (
      match cmd with
      | Skip -> St env
      | Assign (x, e) ->
          St (fun y -> if y=x then eval_expr env e else env y)
      | If (e, c1, c2) -> (
          match eval_expr env e with
          | Bool true -> Cmd (c1, env)
          | Bool false -> Cmd (c2, env)
          | _ -> raise NoRuleApplies)
      | While (e, c) -> (
          match eval_expr env e with
          | Bool true -> Cmd (Seq (c, cmd), env)
          | Bool false -> St env
          | _ -> raise NoRuleApplies)
      | Seq (c1, c2) -> (
          match trace1 (Cmd (c1, env)) with
          | St env' -> Cmd (c2, env')
          | Cmd (c1', env') -> Cmd (Seq (c1', c2), env')))
      ;;

let p c = let conf0 = Cmd(c,s0) in string_of_conf (vars_of_cmd c) (trace1 conf0);;

let eval c = string_of_conf (vars_of_cmd c) (trace1(Cmd(c,s0)));;

let trace n cmd =
  let conf0 = Cmd (cmd, s0) in
  let rec helper i conf =
    if i >= n then [ conf ]
    else
      try conf :: helper (i + 1) (trace1 conf) with NoRuleApplies -> [ conf ]
  in
  helper 0 conf0

  let full s = parse s |> trace 1000 |> string_of_trace (vars_of_cmd (parse s));;
open Ast
open Types
open Prettyprint
open Printf

let parse (s:string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in 
  ast
;;

let get_loc = function
  | BVar l | IVar l -> l
;;


let rec eval_expr (s:state) = function
  | True -> Bool true
  | False -> Bool false
  | Var x -> getmem s (get_loc((topenv s) x))
  | Const x -> Int x
  | Not e -> (match eval_expr s e with 
                | Bool b -> Bool (not b)
                | Int _ -> raise (TypeError "Expected type was bool, but int was provided" ))
  | And(e1,e2) -> (match (eval_expr s e1, eval_expr s e2) with 
                    | Bool b1, Bool b2 -> Bool (b1 && b2)
                    | _ -> raise (TypeError "Expected type was bool, but int was provided" ))
  | Or(e1,e2) -> (match (eval_expr s e1, eval_expr s e2) with 
                    | Bool b1, Bool b2 -> Bool (b1 || b2)
                    | _ -> raise (TypeError "Expected type was bool, but int was provided" ))
  | Add(e1,e2) -> (match (eval_expr s e1, eval_expr s e2) with 
                    | Int i1, Int i2 -> Int (i1 + i2)
                    | _ -> raise (TypeError "Expected type was int, but bool was provided" ))
  | Sub(e1,e2) -> (match (eval_expr s e1, eval_expr s e2) with 
                    | Int i1, Int i2 -> Int (i1 - i2)
                    | _ -> raise (TypeError "Expected type was int, but bool was provided" ))
  | Mul(e1,e2) -> (match (eval_expr s e1, eval_expr s e2) with 
                    | Int i1, Int i2 -> Int (i1 * i2)
                    | _ -> raise (TypeError "Expected type was int, but bool was provided" ))
  | Eq(e1,e2) -> if eval_expr s e1 = eval_expr s e2 then Bool true else Bool false
  | Leq(e1,e2) -> (match (eval_expr s e1, eval_expr s e2) with 
                    | Int i1, Int i2 -> Bool (i1 <= i2)
                    | _ -> raise (TypeError "Expected type was int, but bool was provided" ))
;;


let rec eval_decl (s:state) = function
  | EmptyDecl -> s
  | IntVar(var,d) -> let e = fun x -> if x=var then IVar(getloc s) else (topenv s) x in let s' = (e::popenv s, getmem s, getloc s + 1) in eval_decl s' d
  | BoolVar(var,d) -> let e = fun x -> if x=var then BVar(getloc s) else (topenv s) x in let s' = (e::popenv s, getmem s, getloc s + 1) in eval_decl s' d
;;


let rec trace1 = function
  | St _ -> raise NoRuleApplies
  | Cmd(cmd,env) -> (match cmd with 
    | Skip -> St env
    | Assign(var,e) -> (match env with 
      | (el,m,l) -> St (el,(fun y -> if y = get_loc((topenv env) var) then eval_expr env e else m y),l))
    | Seq(c1,c2) -> 
      (match trace1(Cmd(c1,env)) with
        | St s -> Cmd(c2,s)
        | Cmd(c1',s) -> Cmd(Seq(c1',c2),s))
    | If(e,c1,c2) -> 
        (match eval_expr env e with
          | Bool true -> Cmd(c1,env)
          | Bool false -> Cmd(c2,env)
          | _ -> raise NoRuleApplies)
    | While(e,c) -> 
      (match eval_expr env e with
        | Bool true -> Cmd(Seq(c,cmd),env)
        | Bool false -> St env
        | _ -> raise NoRuleApplies)
    | Decl(d,c) -> let env' = (topenv env::getenv env,getmem env,getloc env) in
      Cmd(Block c, eval_decl env' d)
    | Block c -> (match trace1(Cmd(c,env)) with
      | St s -> St(popenv s,getmem s,getloc s)
      | Cmd(c',s) -> Cmd(Block c', s)));;
    


let env0 x = raise (UnboundVar x);;
let mem0 x = raise (UnboundVar (string_of_int x))

let (s0:state) = ([env0],mem0,0)

let trace n cmd =
  let conf0 = Cmd (cmd, s0) in
  let rec helper i conf =
    if i >= n then [ conf ]
    else
      try conf :: helper (i + 1) (trace1 conf) with NoRuleApplies -> [ conf ]
  in
  helper 0 conf0;;
  


let write_to_file output_file content =
  let channel = open_out output_file in
  fprintf channel "%s\n" content;
  close_out channel

let full st = parse st |> trace 10 |> string_of_trace (vars_of_cmd (parse st)) |> write_to_file "output.txt";;


let dec = IntVar ("z", IntVar ("y", IntVar ("x", EmptyDecl)));;

let prova = (string_of_state (eval_decl s0 dec) (vars_of_decl dec));;

let env1 = fun y -> if y="z" then IVar 0 else if y="y" then IVar 1 else if y="x" then IVar 2 else failwith "";;
let mem1 = fun y -> if y=0 then Int 10 else if y=1 then Int 20 else if y=2 then Int 30 else failwith "";;

let s1 = ([env1],mem1,3);;
let prova1 = string_of_state ([env1],mem1,3) ["z";"y";"x"];;

let prova2 = getmem s1 (get_loc((topenv s1) "z"));;
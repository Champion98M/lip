open Ast
open Types
(*open Prettyprint
open Printf*)

let parse (s:string) : prog =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.start Lexer.read lexbuf in 
  ast
;;


let read_fun name state = match ((topenv state) name) with
  | IFun (par,def,ret) -> par,def,ret
  | _ -> failwith "Not a function"
;;


let get_loc = function
  IVar l -> l
  | _ -> failwith "Not a var"
;;



let rec eval_decl (s:state) = function
  | EmptyDecl -> s
  | IntVar x -> let env = fun y -> if y=x then IVar(getloc s) else (topenv s) y in (env::popenv s,getmem s,getloc s + 1)
  | Fun(name,par,def,ret) -> let env = fun y -> if y=name then IFun(par,def,ret) else (topenv s) y in let s' = (env::popenv s,getmem s,getloc s) in s'
  | DSeq(d1,d2) -> eval_decl (eval_decl s d1) d2
;;

let rec trace_expr (s:state) = function
  | Var x -> (match ((topenv s) x) with
    | IVar l -> (Const(getmem s l), s)
    | IFun _ -> failwith "function")
  | Not e -> (match e with
    | False -> (True, s)
    | True -> (False, s) 
    | _ -> let e', s' = trace_expr s e in (Not e',s'))
  | And(e1,e2) -> (match e1 with
    | False -> (False, s)
    | True -> (e2, s)
    | _ -> let e1', s' = trace_expr s e1 in (And(e1',e2), s'))
  | Or(e1,e2) -> (match e1 with
    | False -> (e2,s)
    | True -> (True,s)
    | e1 -> let e1', s' = trace_expr s e1 in (Or(e1',e2),s')) 
  | Add(e1,e2) -> (match (e1,e2) with
    | Const c1, Const c2 -> (Const(c1+c2), s)
    | Const _, e2 -> let e2', s' = trace_expr s e2 in (Add(e1,e2'), s')
    | e1, __ -> let e1', s' = trace_expr s e1 in (Add(e1',e2),s'))
  | Sub(e1,e2) -> (match (e1,e2) with
    | Const c1, Const c2 -> (Const(c1-c2), s)
    | Const _, e2 -> let e2', s' = trace_expr s e2 in (Sub(e1,e2'), s')
    | e1, __ -> let e1', s' = trace_expr s e1 in (Sub(e1',e2),s'))
  | Mul(e1,e2) -> (match (e1,e2) with
    | Const c1, Const c2 -> (Const(c1*c2), s)
    | Const _, e2 -> let e2', s' = trace_expr s e2 in (Mul(e1,e2'), s')
    | e1, __ -> let e1', s' = trace_expr s e1 in (Mul(e1',e2),s'))
  | Eq(e1,e2) -> (match (e1,e2) with
    | Const c1, Const c2 when c1=c2 -> (True, s)
    | Const _, Const _ -> (False, s)
    | Const _ , e2 -> let e2', s' = trace_expr s e2 in (Eq(e1,e2'),s')
    | e1, _ -> let e1', s' = trace_expr s e1 in (Eq(e1',e2), s'))
  | Leq(e1,e2) -> (match (e1,e2) with
    | Const c1, Const c2 when c1<=c2 -> (True, s)
    | Const _, Const _ -> (False, s)
    | Const _ , e2 -> let e2', s' = trace_expr s e2 in (Leq(e1,e2'),s')
    | e1, _ -> let e1', s' = trace_expr s e1 in (Leq(e1',e2), s'))
  | Call(var,e) -> (match e with
    | Const n -> 
      let par,def,ret = read_fun var s in 
      let s' = let env' = fun y -> if y=par then IVar(getloc s) else (topenv s) y in (env'::getenv s,getmem s,getloc s + 1) in
      let s' = let env' = topenv s' in (getenv s', (fun y -> if (get_loc(env' par))=y then n else (getmem s') y), getloc s') in 
      (CallExec(def,ret),s')
    | e -> let e',s' = trace_expr s e in (Call(var,e'),s'))
  | CallExec(def,ret) -> (match trace_cmd (Cmd(def,s)) with 
    | Cmd(def',env) -> (CallExec(def',ret),env)
    | St env -> (CallRet ret,env))
  | CallRet e -> (match e with
    | Const n -> (Const n, (popenv s, getmem s, getloc s))
    | e -> let e',s' = trace_expr s e in (CallRet e',s'))
  | _ -> raise NoRuleApplies
and eval_expr (s:state) e = match e with
  | True -> (True, s)
  | False -> (False, s)
  | e -> let e',s' = trace_expr s e in eval_expr s' e'
and trace_cmd = function
  | St _ -> raise NoRuleApplies
  | Cmd(cmd,env) -> (match cmd with 
    | Skip -> St env
    | Assign(var,e) -> 
      (match e with
        | Const x -> 
          (match env with 
          | (el,m,l) -> St (el,(fun y -> if y = get_loc((topenv env) var) then x else m y),l))
          | e -> let e', env' = trace_expr env e in (Cmd(Assign(var,e'),env')))
    | If(e,c1,c2) -> 
        (match e with
          | True-> Cmd(c1,env)
          | False -> Cmd(c2,env)
          | e -> let e',env' = trace_expr env e in Cmd(If(e',c1,c2),env'))
    | While(e,c) -> 
      (match eval_expr env e with
        | True, s' -> Cmd(Seq(c,cmd), s')
        | False, s' -> St s'
        | _ -> raise (TypeError "int was expected as a while condition"))
    | Seq(c1,c2) -> 
      (match trace_cmd(Cmd(c1,env)) with
        | St s -> Cmd(c2,s)
        | Cmd(c1',s) -> Cmd(Seq(c1',c2),s)))
;;





let env0 x = raise (UnboundVar x);;
let mem0 x = raise (UnboundVar (string_of_int x));;
  
let (s0:state) = ([env0],mem0,0);;


let trace n (Prog (d, cmd)) =
  let st = eval_decl s0 d in
  let conf0 = Cmd (cmd, st) in
  let rec helper i conf =
    if i >= n then [ conf ]
    else
      try conf :: helper (i + 1) (trace_cmd conf)
      with NoRuleApplies -> [ conf ]
  in
  helper 0 conf0
;;


let apply (s:state) var = match (topenv s) var with
  | IVar l -> getmem s l
  | _ -> failwith "program error"
;;


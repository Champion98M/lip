open Ast

type exprval = Bool of bool | Nat of int;;

let bool_of_expr = function
  | Bool(true) -> true
  | Bool(false) -> false
  | _ -> failwith "Wrong type: expected type was bool, but int was provided"
;;

let int_of_expr = function
 | Nat(n) -> n
 | _ -> failwith "Wrong type: expected type was int, but bool was provided"
;;

let string_of_val = function
  | Bool(true) -> "true"
  | Bool(false) -> "false"
  | Nat(e) -> string_of_int e
;;

let rec is_nv = function
  | Zero -> true
  | Succ e -> is_nv e 
  | _ -> false
;;

let rec string_of_expr = function
    True -> "True"
  | False -> "False"
  | If(e0,e1,e2) -> "If(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Not(e0) -> "Not(" ^ (string_of_expr e0) ^ ")"
  | And(e0,e1) -> "And(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ ","
  | Or(e0,e1) -> "Or(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ ","
  | Zero -> "0"
  | Succ e -> "Succ(" ^ string_of_expr e ^ ")"
  | Pred e -> "Pred(" ^ string_of_expr e ^ ")"
  | IsZero e -> "IsZero(" ^ string_of_expr e ^ ")"
;;


let parse (s:string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in 
  ast
;;

let rec string_of_boolexpr = function
    True -> "True"
  | False -> "False"
  | If(e0,e1,e2) -> "If(" ^ (string_of_boolexpr e0) ^ "," ^ (string_of_boolexpr e1) ^ "," ^ (string_of_boolexpr e2) ^ ")"
  | Not(e0) -> "Not(" ^ (string_of_boolexpr e0) ^ ")"
  | And(e0,e1) -> "And(" ^ (string_of_boolexpr e0) ^ "," ^ (string_of_boolexpr e1) ^ ")"
  | Or(e0,e1) -> "Or(" ^ (string_of_boolexpr e0) ^ "," ^ (string_of_boolexpr e1) ^ ")"
  | Zero -> "0"
  | Succ(e0) -> "Succ(" ^ (string_of_boolexpr e0) ^ ")"
  | Pred(e0) -> "Pred(" ^ (string_of_boolexpr e0) ^ ")"
  | IsZero(e0) -> "IsZero(" ^ string_of_boolexpr e0 ^ ")"
;;


let rec eval = function
  | True -> Bool(true)
  | False -> Bool(false)
  | If(e0,e1,e2) -> if bool_of_expr (eval e0) then eval e1 else eval e2
  | Not(e) -> Bool(not (bool_of_expr(eval e)))
  | And(e0,e1) -> Bool(bool_of_expr (eval e0) && bool_of_expr (eval e1))
  | Or(e0,e1) -> Bool(bool_of_expr (eval e0) || bool_of_expr (eval e1))
  | Zero -> Nat(0)
  | Succ(e) -> Nat(int_of_expr (eval e) + 1)
  | Pred(e) -> let e' = int_of_expr (eval e) in if e' <> 0 then Nat(e' - 1) else failwith "Negative number"
  | IsZero(e) -> if int_of_expr (eval e) = 0 then Bool(true) else Bool(false)
;;

exception NoRuleApplies

let rec trace1 = function 
  | If(True,e1,_) -> e1
  | If(False,_,e2) -> e2
  | If(e0,e1,e2) -> If(trace1 e0,e1,e2)
  | Not(True) -> False
  | Not(False) -> True
  | Not(e0) -> Not(trace1 e0)
  | And(True,e2) -> e2
  | And(False,_) -> False
  | And(e1,e2) -> And(trace1 e1,e2)
  | Or(True,_) -> True
  | Or(False,e2) -> e2
  | Or(e1,e2) -> Or(trace1 e1,e2)
  | Succ e -> Succ(trace1 e)
  | Pred(Succ(nv)) when is_nv nv -> nv 
  | Pred e -> Pred(trace1 e)
  | IsZero(Zero) -> True
  | IsZero(Succ(nv)) when is_nv nv -> False
  | IsZero e -> IsZero(trace1 e)
  | _ -> raise NoRuleApplies
;;

let rec trace e = try
    let e' = trace1 e in
    e::(trace e')
  with NoRuleApplies -> [e]
;;

type exprtype = BoolT | NatT;;

let string_of_type = function
 | BoolT -> "Bool"
 | NatT -> "Nat"
;;

exception TypeError of string;;

let error_msg expr_ actual expected = 
  string_of_expr expr_ ^ " has type " ^ string_of_type actual ^ ", but type " ^ expected^ " was expected"

let rec typecheck = function
  | True | False -> BoolT
  | Not e -> let type_e = typecheck e in if type_e = BoolT then BoolT else raise (TypeError (error_msg e type_e "Bool"))
  | And(e0,e1) | Or(e0,e1) -> 
      let type_e0 = typecheck e0 in
      if type_e0 <> BoolT then raise (TypeError(error_msg e0 type_e0 "Bool")) 
      else let type_e1 = typecheck e1 in
      if type_e1 <> BoolT then raise (TypeError(error_msg e1 type_e1 "Bool"))
      else BoolT
  | If(e0,e1,e2) -> let type_e0 = typecheck e0 in
      if type_e0 <> BoolT then raise (TypeError(error_msg e0 type_e0 "Bool"))
      else let type_e1 = typecheck e1 in
      let type_e2 = typecheck e2 in
      if type_e2 <> type_e1 then raise (TypeError(error_msg e2 type_e2 (string_of_type type_e1)))
      else type_e1
  | Zero -> NatT
  | Succ e | Pred e -> let type_e = typecheck e in 
      if type_e <> NatT then raise (TypeError(error_msg e type_e "Nat"))
      else NatT
  | IsZero e -> let type_e = typecheck e in
      if type_e <> NatT then raise (TypeError(error_msg e type_e "Nat"))
      else BoolT
;;

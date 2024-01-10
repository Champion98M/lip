(* tokens *)
type token = A | B | X

(* val toklist_of_string : string -> token list *)
(* toklist_of_string s transforms the string s into a list of tokens *)
(* Hint: use the function explode in bin/main.ml to convert a string to a char list *)
             
let toklist_of_string s = 
  let rec tok i l =
    if i<0 then l else tok (i-1) (if s.[i] = 'A' then A::l else if s.[i] = '=' then X::l else if s.[i] = 'B' then B::l else failwith "Invalid string") in 
  tok (String.length s - 1) []

(* val valid : token list -> bool *)
(* valid l is true when l is a list of tokens in the language A* X* B* *)
    
let valid l = 
  let rec val_ q = function
    | [] -> if q=3 then true else false
    | A::l -> if q=0 || q=1 then val_ 1 l else false
    | X::l -> if q=1 || q=2 then val_ 2 l else false
    | B::l -> if q=2 || q=3 then val_ 3 l else false in 
  val_ 0 l

(* val win : token list -> token *)
(* win l determines the winner of a tug of war game. X means tie *)

let win l = 
  let countA = List.length (List.filter (fun x -> x = A) l) in 
  let countB = List.length (List.filter (fun x -> x = B) l) in 
  if countA = countB then X else if countA-countB > 0 then A else B

(* val string_of_winner : token -> string *)

let string_of_winner w = match w with
 | A -> "A won"
 | B -> "B won"
 | X -> "Draw"

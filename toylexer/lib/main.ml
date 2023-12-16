open Token
    
(* tokenize : Lexing.lexbuf -> LexingLib.Token.token list *)

let rec tokenize lexbuf =
  match Lexer.read_token lexbuf with
    EOF -> [EOF]
  | t -> t::(tokenize lexbuf)

(* lexer : string -> LexingLib.Token.token list *)

let lexer (s : string) =
  let lexbuf = Lexing.from_string s in
  tokenize lexbuf

(* string_of_tokenlist : token list -> string *)
    
let string_of_tokenlist tl = 
  List.fold_left (fun s t -> s ^ (string_of_token t ^ (if t=EOF then "" else " "))) "" tl

(* string_of_frequencies : (token * int) list -> string *)
    
let string_of_frequencies fl =
  List.fold_left (fun s (t,n) -> s ^ ((string_of_token t) ^ " -> " ^ string_of_int n ^ "\n")) "" fl

(* frequency : int -> 'a list -> ('a * int) list *)
let frequency n tl = 
  let rec freq = function
    | [] -> []
    | t::_l -> (t, List.length (List.filter (fun x -> x=t) tl))::freq (List.filter (fun x -> x<>t) _l) in 
  let rec extract n l = match n, l with
    | 0,_ | _,[] -> []
    | n,hd::tl -> hd::extract (n-1) tl in
  extract n (List.sort (fun (_,a) (_,b) -> b-a) (freq tl));;




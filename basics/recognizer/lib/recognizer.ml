let lang1 w = 
    let rec lan1 q = function 
      | [] -> if q=1 then true else false
      | '0'::l | '1'::l -> lan1 1 l
      | _ -> false in 
  lan1 0 w;;


let lang2 w = 
  let rec lan2 q = function
    | [] -> if q=0 || q=1 then true else false
    | '0'::l -> if q=0 then lan2 1 l else false
    | '1'::l -> lan2 1 l
    | _ -> false in 
  lan2 0 w;;


let lang3 w = 
  let rec lan3 q = function
    | [] -> if q=2 then true else false
    | '0'::l -> if q=0 || q=1 then lan3 (q+1) l else lan3 q l
    | '1'::l -> if q=0 then false else lan3 1 l
    | _ -> false in 
  lan3 0 w;;


let lang4 w = 
  let rec lan4 q = function
    | [] -> if q=2 then true else false
    | '0'::l -> lan4 q l
    | '1'::l -> if q=2 then false else lan4 (q+1) l
    | _ -> false in 
  lan4 0 w;;


let lang5 w = 
  let rec lan5 q = function
   | [] -> if q=2 then true else false
   | '0'::l -> if q=0 || q=1 then lan5 (q+1) l else if q=2 then lan5 1 l else false
   | '1'::l -> if q=0 then lan5 3 l else if q=3 then lan5 2 l else if q=2 then lan5 3 l else false
   | _ -> false in 
  lan5 0 w;;
    

let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers
  

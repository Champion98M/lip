type rule = Rule of int list * int list


let string_to_rule s = 
  let parts = String.split_on_char '/' s in 
  if List.length parts <> 2 then failwith "Error length" else
    try 
      match parts with
      | [h;t] -> 
          let h_list = List.map (fun x -> int_of_string (String.make 1 x)) (List.tl (List.init (String.length h) (String.get h))) in
          let t_list = List.map (fun x -> int_of_string (String.make 1 x)) (List.tl (List.init (String.length t) (String.get t))) in 
          if String.get h 0 = 'S' && String.get t 0 ='B' then Rule (h_list, t_list) 
          else if String.get h 0 = 'B' && String.get t 0 = 'S' then Rule (t_list, h_list) 
          else failwith "Error"
      | _ -> failwith "Error 2"
    with _ -> failwith "Execption  occurred, write the rule on this format S23/B3"

let get_survival = function
    | Rule (s,_) -> s

let get_born = function
    | Rule (_,b) -> b
  
let rec get_list_rec = function
| (start, end_) when start < end_ -> start::get_list_rec (start+1, end_)
| (start, end_) when start = end_ -> [end_]
| _ -> failwith "Range Error: start should be less or equal than end";;
  
let get_list s = List.fold_left (fun res x -> let xx = String.split_on_char '.' x in
  match List.filter (fun z -> z<>"") xx with 
    | [single] -> res@[int_of_string single]
    | [start; end_] -> let y = (int_of_string start, int_of_string end_) in res@(get_list_rec y)
    | _ -> failwith "Error") [] (String.split_on_char ',' s);;

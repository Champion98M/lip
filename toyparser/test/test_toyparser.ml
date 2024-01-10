open Toyparser.Main
open Toyparser.Ast

let%test "1" = 
  let expected = Add (Const 5, Const 1) in 
  let parse_ast = parse "5+1" in 
  expected = parse_ast

let%test "2" = 
  let expected = Add (Add (Const 1, Const 2), Const 3) in
  let parse_ast = parse "1+2+3" in
  expected = parse_ast

let%test "3" = 
  let expected = Add (Const 1, Add (Const 2, Const 3)) in
  let parse_ast = parse "1+(2+3)" in
  expected = parse_ast

let%test "4" = 
  let expected = Sub (Const 1, Const 2) in
  let parse_ast = parse "1-2" in
  expected = parse_ast

let%test "5" = 
  let expected = Sub (Const 1, Add (Const 2, Const 3)) in
  let parse_ast = parse "1-(2+3)" in
  expected = parse_ast

let%test "6" = 
  let expected = Add (Sub (Const 1, Const 2), Const 3) in
  let parse_ast = parse "1-2+3" in
  expected = parse_ast 

let%test "7" = parse "1+2+3" |> eval |> string_of_result = "6"

let%test "8" = parse "1-(2+3)*2-5" |> eval |> string_of_result = "-14"

let%test "9" = parse "1+2+3*2+2*5" |> eval |> string_of_result = "19"

let%test "10" = parse "1+2+3*2+2/5" |> eval |> string_of_result = "9"

let%test "11" = parse "1+2+3/0+2*5" |> eval |> string_of_result = "Error"

let%test "12" = parse "4/2+3*3-5+3+(5*2-6+3-(5+2/2))*2-1" |> eval |> string_of_result = "10"

let%test "13" = parse "-1-2-3" |> eval |> string_of_result = "-6"

let%test "14" = parse "-1-2--3" |> eval |> string_of_result = "0"

let%test "15" = parse "-1-2--4" |> eval |> string_of_result = "1"

let%test "16" = parse "2+5-(3-6)" |> eval |> string_of_result = "10"

let%test "17" = parse "-(2+4+6)" |> eval |> string_of_result = "-12"

let%test "18" = parse "-(-2-4-6)" |> eval |> string_of_result = "12"

let%test "19" = parse "0xf*0x02" |> eval |> string_of_result = "30"

let%test "20" = parse "-(0xa)" |> eval |> string_of_result = "-10"

let%test "21" = parse "0x01+2" |> eval |> string_of_result = "3"

let%test "22" = parse "2*5/2" |> eval |> string_of_result = "5"
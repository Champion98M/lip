open Tugofwar

let%test "1" = toklist_of_string "AA=BBB" = [A;A;X;B;B;B]

let%test "2" = valid (toklist_of_string "AA=BBB") = true
let%test "3" = valid (toklist_of_string "BB=AAA") = false

let%test "4" = win (toklist_of_string "AA=B") = A
let%test "5" = win (toklist_of_string "A=BB") = B
let%test "6" = win (toklist_of_string "A=B") = X

let%test "7" = string_of_winner A = "A won"
let%test "8" = string_of_winner B = "B won"
let%test "0" = string_of_winner X = "Draw"
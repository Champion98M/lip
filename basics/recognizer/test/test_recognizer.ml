open Recognizer

let%test "1" =
  belongsTo ['0';'1';'1';'0';'1'] = [true;false;false;false;false]

let%test "2" =
  belongsTo ['0';'0';'1';'0';'1'] = [true;false;false;true;false]

  let%test "3" =
  belongsTo ['0';'1';'1';'1';'1'] = [true;true;false;false;false]

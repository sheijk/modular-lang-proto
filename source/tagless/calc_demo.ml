
let int_tests (type t) (module L : Calc_int.Lang with type t = t) : (int * t) list =
  L.[
    3, int 1 + int 2;
    1, int 1;
    99, int 99;
    106, int 100 + int 3 * int 2;
  ]

let test_int() =
  List.iter2 (fun (expected, string) (_, f) ->
      let result = f() in
      if expected = result then
        Printf.printf "ok  %s => %d\n" string result
      else
        Printf.printf "err %s => %d, expected %d\n" string result expected)
    (int_tests (module Calc_int.To_string))
    (int_tests (module Calc_int.Eval))

module Tests(L : Calc_bool.Lang) =
struct
  let tests = L.[
      true, bool true;
      false, bool false;
      true, (bool true && bool true);
      false, (bool true && bool false);
      true, (bool true || bool false);
      true, (bool false || bool true);
      false, (bool false || bool false);
      true, (bool true && (bool true || bool false));
      false, (bool true && bool false || bool false && bool true);
    ]
end

let test_bool () =
  let module C = Tests(Calc_bool.To_string) in
  let module E = Tests(Calc_bool.Eval) in
  List.iter2 (fun (expected, string) (_, f) ->
      let result = f() in
      if expected = result then
        Printf.printf "ok  %s => %b\n" string result
      else
        Printf.printf "err %s => %b, expected %b\n" string result expected)
    C.tests E.tests

let () =
  print_endline "hello, tagless Calc";
  test_bool ();
  test_int ()




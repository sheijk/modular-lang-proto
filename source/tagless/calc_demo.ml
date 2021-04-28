
module Tests_int(L : Calc_int.Lang) =
struct
  let tests = L.[
      3, int 1 + int 2;
      1, int 1;
      99, int 99;
      106, int 100 + int 3 * int 2;
    ]
end

let test_int() =
  let module C = Tests_int(Calc_int.To_string) in
  let module E = Tests_int(Calc_int.Eval) in
  List.iter2 (fun (expected, string) (_, f) ->
      let result = f() in
      if expected = result then
        Printf.printf "ok  %s => %d\n" string result
      else
        Printf.printf "err %s => %d, expected %d\n" string result expected)
    C.tests E.tests

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




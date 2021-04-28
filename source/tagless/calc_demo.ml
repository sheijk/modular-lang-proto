
module Status : sig
  val log_test_failure : unit -> unit
  val finish : unit -> unit
end = struct
  let errors = ref 0
  let log_test_failure() = incr errors
  let finish () =
    if ((!errors) > 0) then begin
      Printf.printf "%d tests failed\n" (!errors);
      exit 1;
    end
end

module Tests_int(L : Calc_int.Lang) =
struct
  let tests = L.[
      3, int 1 +. int 2;
      1, int 1;
      99, int 99;
      106, int 100 +. int 3 *. int 2;
    ]
end

let test_int() =
  print_endline "Testing Calc_int";
  let module C = Tests_int(Calc_int.To_string) in
  let module E = Tests_int(Calc_int.Eval) in
  List.iter2 (fun (expected, string) (_, f) ->
      let result = f (Eval_base.new_context()) in
      if expected = result then
        Printf.printf "  ok %s => %d\n" string result
      else begin
        Status.log_test_failure();
        Printf.printf "  err %s => %d, expected %d\n" string result expected;
      end)
    C.tests E.tests

module Tests_bool(L : Calc_bool.Lang) =
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
  print_endline "Testing Calc_bool";
  let module C = Tests_bool(Calc_bool.To_string) in
  let module E = Tests_bool(Calc_bool.Eval) in
  List.iter2 (fun (expected, string) (_, f) ->
      let result = f (Eval_base.new_context ()) in
      if expected = result then
        Printf.printf "  ok %s => %b\n" string result
      else begin
        Status.log_test_failure();
        Printf.printf "  err %s => %b, expected %b\n" string result expected;
      end)
    C.tests E.tests


module Tests_combined(L : Calc.Lang) =
struct
  let int_tests =
    let module Ci = Tests_int(L) in
    Ci.tests @ L.[
      1, int 1;
      15, int 10 +. int 5;
      123, (int 1 *. int 10 +. int 2) *. int 10 +. int 3;
      (* 5, int 10 / int 2; *)
    ]

  let bool_tests =
    let module Cb = Tests_bool(L) in
    Cb.tests @ L.[
      true, bool true;
      true, int 4 <. int 10;
      false, int 4 >. int 10;
      true, int 3 =. int 3;
      false, int 3 =. int 4;
      true, int 3 >. int (-10);
      false, int 3 >. int 3;

      true, bool true || bool false;
      false, bool false || bool false;
      false, bool true && bool false;
    ]

  (* Float tests from ast/Calc *)
  (*   run (i 8) Build.(to_i (f 8.)); *)
  (*   run (i 74) Build.(to_i (f 64.) + i 10); *)
  (*   run (f 8.) Build.(f 13. - f 5.); *)
  (*   run (f 10.) Build.(to_f @@ i 8 + i 2); *)
  (*   run (f 10.) Build.(f 8. + to_f (i 2)); *)
  (*   run (f 24.) Build.(to_f (i 16) + f 8.); *)
  (*   run (f 8.) Build.(f 3. + i 5); *)
end

let test_combined () =
  print_endline "Testing Calc";
  let module C = Tests_combined(Calc.To_string) in
  let module E = Tests_combined(Calc.Eval) in
  List.iter2 (fun (expected, string) (_, f) ->
      let result = f (Eval_base.new_context ()) in
      if expected = result then
        Printf.printf "  ok %s => %d\n" string result
      else begin
        Status.log_test_failure();
        Printf.printf "  err %s => %d, expected %d\n" string result expected;
      end)
    C.int_tests E.int_tests;
  List.iter2 (fun (expected, string) (_, f) ->
      let result = f (Eval_base.new_context ()) in
      if expected = result then
        Printf.printf "  ok %s => %b\n" string result
      else begin
        Status.log_test_failure();
        Printf.printf "  err %s => %b, expected %b\n" string result expected;
      end)
    C.bool_tests E.bool_tests


module Tests_algo(L : Algo.Lang) =
struct
  let int_tests =
    let module C = Tests_combined(L) in
    List.map (fun (x, t) -> Some x, t) C.int_tests
    @ L.[
        Some 10, int 3 +. int 7;
        (* 5, int 10 / int 2; *)
        Some 3, if_ (int 10 >. int 20) (int 666) (int 3);
        (* *   run exn Build.(loop (i 0)); *)
        (* *   run exn Build.(break (i 3)); *)
        Some 10, (loop (break (int 10)));
        (* 11, (loop (cond (index >. int 10) (break index) (int 1))); *)
        None, loop (int 1);
      ]

  let bool_tests =
    let module C = Tests_combined(L) in
    List.map (fun (x, t) -> Some x, t) C.bool_tests
    @ L.[
      Some true, bool true;
      (* true, int 1 =. loop (int 1); *)
    ]

 (* *   run (i 2) Build.(cond (i 0) (i 1) (i 2));
  * *   run (i 1) Build.(cond (i 1) (i 1) (i 2));
  * *   run (i 22) Build.(cond (i 1 - i 1) (i 1 + i 10) (i 2 + i 20));
  * *   run (i 11) Build.(cond (f 2. - f 1.) (i 1 + i 10) (i 2 + i 20));
  * *   run (i 1) Build.(cond (b true) (i 1) (i 999)); *)
end

let test_algo () =
  print_endline "Testing Algo";
  let fail testcase expected result =
    Status.log_test_failure();
    Printf.printf "  err %s => %s, expected %s\n" testcase result expected;
  in
  let ok testcase result =
    Printf.printf "  ok %s => %s\n" testcase result
  in
  let to_result_str f opt =
    Option.value ~default:"error" @@ Option.map f opt
  in
  let run string expected f to_string =
    let result =
      try Some (f (Eval_base.new_context ()))
      with _ -> None
    in
    let to_result_str = to_result_str to_string in
    if Option.equal (=) expected result then
      ok string (to_result_str result)
    else
      fail string (to_result_str expected) (to_result_str result)
  in
  let module C = Tests_algo(Algo.To_string) in
  let module E = Tests_algo(Algo.Eval) in
  List.iter2 (fun (expected, string) (_, f) ->
      run string expected f string_of_int)
    C.int_tests E.int_tests;
  List.iter2 (fun (expected, string) (_, f) ->
      run string expected f string_of_bool)
    C.bool_tests E.bool_tests

let () =
  print_endline "hello, tagless Calc";
  test_bool ();
  test_int ();
  test_combined ();
  test_algo ();
  Status.finish ()


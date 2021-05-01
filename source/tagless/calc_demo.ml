
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

let fail testcase expected result =
  Status.log_test_failure();
  Printf.printf "  err %s => %s, expected %s\n" testcase result expected

let ok testcase result =
  Printf.printf "  ok %s => %s\n" testcase result

let to_result_str f opt =
  Option.value ~default:"error" @@ Option.map f opt

let run string expected f to_string =
  let result =
    try Some (f (Interpreter_context.make ()))
    with _ -> None
  in
  let to_result_str = to_result_str to_string in
  if Option.equal (=) expected result then
    ok string (to_result_str result)
  else
    fail string (to_result_str expected) (to_result_str result)

module Tests_int(L : Calc_int.Layer.Lang) =
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
  let module C = Tests_int(Calc_int.Full.To_string) in
  let module E = Tests_int(Calc_int.Full.Eval) in
  List.iter2 (fun (expected, string) (_, f) ->
      let result = f (Interpreter_context.make ()) in
      if expected = result then
        Printf.printf "  ok %s => %d\n" string result
      else begin
        Status.log_test_failure();
        Printf.printf "  err %s => %d, expected %d\n" string result expected;
      end)
    C.tests E.tests

module Tests_bool(L : Calc_bool.Layer.Lang) =
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
  let module C = Tests_bool(Calc_bool.Full.To_string) in
  let module E = Tests_bool(Calc_bool.Full.Eval) in
  List.iter2 (fun (expected, string) (_, f) ->
      let result = f (Interpreter_context.make ()) in
      if expected = result then
        Printf.printf "  ok %s => %b\n" string result
      else begin
        Status.log_test_failure();
        Printf.printf "  err %s => %b, expected %b\n" string result expected;
      end)
    C.tests E.tests


module Tests_combined(L : Calc.Layer.Lang) =
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
  let module C = Tests_combined(Calc.Full.To_string) in
  let module E = Tests_combined(Calc.Full.Eval) in
  List.iter2 (fun (expected, string) (_, f) ->
      let result = f (Interpreter_context.make ()) in
      if expected = result then
        Printf.printf "  ok %s => %d\n" string result
      else begin
        Status.log_test_failure();
        Printf.printf "  err %s => %d, expected %d\n" string result expected;
      end)
    C.int_tests E.int_tests;
  List.iter2 (fun (expected, string) (_, f) ->
      let result = f (Interpreter_context.make ()) in
      if expected = result then
        Printf.printf "  ok %s => %b\n" string result
      else begin
        Status.log_test_failure();
        Printf.printf "  err %s => %b, expected %b\n" string result expected;
      end)
    C.bool_tests E.bool_tests


module type Algo_calc =
sig
  type 'a t
  include Calc.Layer.Lang with type 'a t := 'a t
  include Algo.Layer.Lang with type 'a t := 'a t
end

module Tests_algo(L : Algo_calc) =
struct
  let int_tests =
    let module C = Tests_combined(L) in
    List.map (fun (x, t) -> Some x, t) C.int_tests
    @ L.[
        Some 10, int 3 +. int 7;
        (* 5, int 10 / int 2; *)
        Some 3, if_ (int 10 >. int 20) (int 666) (int 3);
        None, (loop (int 0));
        None, (break (int 3));
        Some 10, (loop (break (int 10)));
        Some 11, (loop (if_ (loop_index() >. int 10) (break @@ loop_index()) (int 1)));
        None, loop (int 1);
        Some 2, (if_ (bool false) (int 1) (int 2));
        Some 1, (if_ (bool true) (int 1) (int 2));
        Some 22, (if_ (int 1 -. int 1 >. int 1) (int 1 +. int 10) (int 2 +. int 20));
        Some 11, (if_ (int 2 -. int 1 <. int 2) (int 1 +. int 10) (int 2 +. int 20));
        Some 1, (if_ (bool true) (int 1) (int 999));
      ]

  let bool_tests =
    let module C = Tests_combined(L) in
    List.map (fun (x, t) -> Some x, t) C.bool_tests
    @ L.[
      Some true, bool true;
      (* true, int 1 =. loop (int 1); *)
    ]
end

let test_algo () =
  print_endline "Testing Algo";
  let module C = Tests_algo(Algo.Full.To_string) in
  let module E = Tests_algo(Algo.Full.Eval) in
  List.iter2 (fun (expected, string) (_, f) ->
      run string expected f string_of_int)
    C.int_tests E.int_tests;
  List.iter2 (fun (expected, string) (_, f) ->
      run string expected f string_of_bool)
    C.bool_tests E.bool_tests

module Algo_bool =
struct
  module type Lang = sig
    type 'a t
    include Algo.Layer.Lang with type 'a t := 'a t
    include Calc_bool.Layer.Lang with type 'a t := 'a t
  end

  module To_string = struct
    type 'a t = string
    include Calc_bool.Layer.To_string
    include Algo.Layer.To_string
  end

  module Eval = struct
    include Empty.Eval
    include Calc_bool.Layer.Eval
    include Algo.Layer.Eval
  end
end

module Tests_algo_bool(L : Algo_bool.Lang) =
struct
  let bool_tests =
    L.[
        Some true, bool true;
        Some true, bool true || bool false;
        Some false, bool false || bool false;
        Some false, bool true && bool false;
      ]
end

let test_algo_bool () =
  print_endline "Testing Algo_bool";
  let module C = Tests_algo_bool(Algo_bool.To_string) in
  let module E = Tests_algo_bool(Algo_bool.Eval) in
  List.iter2 (fun (expected, string) (_, f) ->
      run string expected f string_of_bool)
    C.bool_tests E.bool_tests

let () =
  print_endline "hello, tagless Calc";
  test_bool ();
  test_int ();
  test_combined ();
  test_algo ();
  test_algo_bool ();
  Status.finish ()


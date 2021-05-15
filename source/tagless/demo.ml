
module Tester_stats =
struct
  let errors = ref 0
  let total = ref 0

  let fail testcase expected result =
    incr total;
    incr errors;
    Printf.printf "  err %s => %s, expected %s\n" testcase result expected

  let ok testcase result =
    incr total;
    Printf.printf "  ok %s => %s\n" testcase result

  let finish () =
    if ((!errors) > 0) then begin
      Printf.printf "%d tests run, %d tests failed" (!total) (!errors);
      exit 1
    end else begin
      Printf.printf "%d tests run\n" (!total);
      exit 0
    end
end

module Tester_f(I : Interpreter.Create) : sig
  val finish : unit -> unit
  val run :
    string ->
    I.value option ->
    (I.t -> I.value) ->
    (I.value -> string) ->
    unit
end = struct
  include Tester_stats

  let finish = Tester_stats.finish

  let to_result_str f opt =
    Option.value ~default:"error" @@ Option.map f opt

  let run string expected f to_string =
    let result =
      try Some (f (I.make ()))
      with _ -> None
    in
    let to_result_str = to_result_str to_string in
    if Option.equal (=) expected result then
      ok string (to_result_str result)
    else
      fail string (to_result_str expected) (to_result_str result)
end

module Tester = Tester_f(Interpreter.Dynamic)
module Tester_legacy = Tester_f(Interpreter.No_runtime)

module type Test_names =
sig
  type 'a t = string
  type value

  val int_tests : (value option * string) list
  val bool_tests : (value option * string) list
end

module type Test_cases =
sig
  module I : Interpreter.Create

  type 'a t = I.t -> I.value
  val int_tests : (I.value option * int t) list
  val bool_tests : (I.value option * bool t) list
end

module Test_runner(C : Test_names)(E : Test_cases with type I.value = C.value) =
struct
  let run case_name =
    let module Tester = Tester_f(E.I) in
    Printf.printf "Testing %s\n" case_name;
    List.iter2 (fun (expected, string) (_, f) ->
        Tester.run string expected f E.I.value_string)
      C.int_tests E.int_tests;
    List.iter2 (fun (expected, string) (_, f) ->
        Tester.run string expected f E.I.value_string)
      C.bool_tests E.bool_tests
end

module Tests_int(L : Calc_int.Lang) =
struct
  type 'a t = 'a L.t
  type value = Interpreter.Default_values.value
  module I = Interpreter.No_runtime

  let is_int n = Some (I.int n)

  let int_tests = L.[
      is_int 3, int 1 +. int 2;
      is_int 1, int 1;
      is_int 99, int 99;
      is_int 106, int 100 +. int 3 *. int 2;
    ]

  let bool_tests = []
end

let test_int() =
  let module T =
    Test_runner
      (Tests_int(Calc_int.To_string))
      (Tests_int(Calc_int.Eval(Interpreter.No_runtime)))
  in
  T.run "Calc_int"

module Tests_bool(L : Calc_bool.Lang) =
struct
  type 'a t = 'a L.t
  module I = Interpreter.No_runtime
  type value = Interpreter.Default_values.value

  let is_bool b = Some (I.bool b)

  let bool_tests = L.[
      is_bool true, bool true;
      is_bool false, bool false;
      is_bool true, (bool true && bool true);
      is_bool false, (bool true && bool false);
      is_bool true, (bool true || bool false);
      is_bool true, (bool false || bool true);
      is_bool false, (bool false || bool false);
      is_bool true, (bool true && (bool true || bool false));
      is_bool false, (bool true && bool false || bool false && bool true);
    ]

  let int_tests = []
end

let test_bool () =
  let module T =
    Test_runner
      (Tests_bool(Calc_bool.To_string))
      (Tests_bool(Calc_bool.Eval(Interpreter.No_runtime)))
  in
  T.run "Calc_bool"

module Tests_combined(L : Calc.Lang) =
struct
  type 'a t = 'a L.t
  module I = Interpreter.No_runtime
  type value = Interpreter.Default_values.value

  let is_int n = Some (I.int n)
  let is_bool b = Some (I.bool b)

  let int_tests =
    let module Ci = Tests_int(L) in
    Ci.int_tests @ L.[
        is_int 1, int 1;
        is_int 15, int 10 +. int 5;
        is_int 123, (int 1 *. int 10 +. int 2) *. int 10 +. int 3;
      (* Some 5, int 10 / int 2; *)
    ]

  let bool_tests =
    let module Cb = Tests_bool(L) in
    Cb.bool_tests @ L.[
      is_bool true, bool true;
      is_bool true, int 4 <. int 10;
      is_bool false, int 4 >. int 10;
      is_bool true, int 3 =. int 3;
      is_bool false, int 3 =. int 4;
      is_bool true, int 3 >. int (-10);
      is_bool false, int 3 >. int 3;

      is_bool true, bool true || bool false;
      is_bool false, bool false || bool false;
      is_bool false, bool true && bool false;
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

let test_combined() =
  let module T =
    Test_runner
      (Tests_combined(Calc.To_string))
      (Tests_combined(Calc.Eval(Interpreter.No_runtime)))
  in
  T.run "Calc"

module Tests_algo(L : Algo_calc.Lang) =
struct
  type 'a t = 'a L.t
  module I = Interpreter.Dynamic
  type value = Interpreter.Default_values.value

  let is_int n = Some (I.int n)
  let is_bool b = Some (I.bool b)

  let int_tests =
    let module C = Tests_combined(L) in
    C.int_tests @ L.[
        is_int 10, int 3 +. int 7;
        is_int 5, int 10 /. int 2;
        is_int 3, if_ (int 10 >. int 20) (int 666) (int 3);
        None, (loop (int 0));
        None, (break (int 3));
        is_int 10, (loop (break (int 10)));
        is_int 11, (loop (if_ (loop_index() >. int 10) (break @@ loop_index()) (int 1)));
        None, loop (int 1);
        is_int 2, (if_ (bool false) (int 1) (int 2));
        is_int 1, (if_ (bool true) (int 1) (int 2));
        is_int 22, (if_ (int 1 -. int 1 >. int 1) (int 1 +. int 10) (int 2 +. int 20));
        is_int 11, (if_ (int 2 -. int 1 <. int 2) (int 1 +. int 10) (int 2 +. int 20));
        is_int 1, (if_ (bool true) (int 1) (int 999));
      ]

  let bool_tests =
    let module C = Tests_combined(L) in
    C.bool_tests @ L.[
      is_bool true, bool true;
      (* true, int 1 =. loop (int 1); *)
    ]
end

let test_algo() =
  let module T =
    Test_runner
      (Tests_algo(Algo_calc.To_string))
      (Tests_algo(Algo_calc.Eval(Interpreter.Dynamic)))
  in
  T.run "Algo_calc"

module Tests_algo_bool(L : Algo_bool.Lang) =
struct
  type 'a t = 'a L.t
  type value = Interpreter.Default_values.value
  module I = Interpreter.Dynamic

    let is_bool b = Some (I.bool b)

    let bool_tests =
    L.[
        is_bool true, bool true;
        is_bool true, bool true || bool false;
        is_bool false, bool false || bool false;
        is_bool false, bool true && bool false;
      ]

  let int_tests = []
end

let test_algo_bool() =
  let module T =
    Test_runner
      (Tests_algo_bool(Algo_bool.To_string))
      (Tests_algo_bool(Algo_bool.Eval(Interpreter.Dynamic)))
  in
  T.run "Algo_bool"

module Tests_algo_bindings(L : Algo_bindings.Lang) =
struct
  type 'a t = 'a L.t
  type value = Interpreter.Default_values.value
  module I = Interpreter.Dynamic

  let is_int n = Some (I.int n)
  let is_bool b = Some (I.bool b)

  let bool_tests =
    let module C = Tests_algo(L) in
    C.bool_tests @ []

  let int_tests =
    let module C = Tests_algo(L) in
    C.int_tests @ L.[
        is_int 99, let_ "foo" (int 99) (get "foo");
        is_int 123,
        let_ "foo" (int (-1))
          (get "foo" +.
           let_ "foo" (int 24)
             (int 100 +. get "foo"));

        is_int 14,
        let_ "sum" (int 0)
          (loop
             (set "sum" (loop_index() *. loop_index() +. get "sum")
                (if_ (get "sum" >. int 10)
                   (break (get "sum"))
                   (int 1))))
      ]
end

let test_algo_bindings () =
  let module T =
    Test_runner
      (Tests_algo_bindings(Algo_bindings.To_string))
      (Tests_algo_bindings(Algo_bindings.Eval(Interpreter.Dynamic)))
  in
  T.run "Algo_bindings"

module Tests_algo_compiler_errors(L : Algo_bindings.Lang) =
struct
  type 'a t = 'a L.t

  let is_int n = Some (Interpreter.Default_values.int n)
  let is_bool b = Some (Interpreter.Default_values.bool b)

  let bool_tests =
    let module B = Tests_algo_bindings(L) in
    B.bool_tests @ []

  let int_tests =
    let module B = Tests_algo_bindings(L) in
    B.int_tests @ L.[
      None, if_ (bool false) (loop_index()) (int 0);
      None,
      let_ "x" (int 10)
        (get "x" +. get "y")
    ]
end

let test_algo_compiled () =
  print_endline "Testing Algo_calc compiled";
  let module P = Tests_algo_compiler_errors(Algo_bindings.To_string) in
  let module C = Tests_algo_compiler_errors(Algo_bindings.Eval_compiled) in
  let check_and_run info f ctx =
    let f = f @@ Compiler.Context.make () in
    if false = Compiler.Info.validate info then
      failwith "compiler error"
    else
      f ctx
  in
  let value_string = Interpreter.Default_values.value_string in
  List.iter2 (fun (expected, string) (_, (info, f)) ->
      Tester_legacy.run string expected (check_and_run info f) value_string)
    P.bool_tests C.bool_tests;
  List.iter2 (fun (expected, string) (_, (info, f)) ->
      Tester_legacy.run string expected (check_and_run info f) value_string)
    P.int_tests C.int_tests

module Tests_algo_optimize(L : Algo_bindings.Lang) =
struct
  type 'a t = 'a L.t

  let bool_tests = L.[
      Some true, bool true;
      Some false, bool false;

      Some true, bool false || bool true;
      Some true, bool true || bool false;
      Some false, bool false || bool false;
      Some true, bool true && bool true;
      Some false, bool true && bool false;

      Some true, bool true && (bool true || bool false);
      Some false, bool true && bool false || bool false && bool true;

      Some false, int 3 =. int 4;
      Some false, int 3 >. int 3;
      Some false, int 4 >. int 10;
      Some true, int 3 =. int 3;
      Some true, int 3 >. int (-10);
      Some true, int 4 <. int 10;
    ]

  let int_tests = L.[
      Some 1, int 1;
      Some 5, int 10 /. int 2;
      Some 10, int 3 +. int 7;
      Some 15, int 10 +. int 5;

      Some 123, (int 1 *. int 10 +. int 2) *. int 10 +. int 3;

      Some 3, if_ (int 10 >. int 20) (int 666) (int 3);
      None, loop (int 0);
      None, loop (int 1);
      None, break (int 3);
      None, loop (break (int 10));
      None, loop (if_ (loop_index() >. int 10) (break @@ loop_index()) (int 1));
      Some 2, if_ (bool false) (int 1) (int 2);
      Some 1, if_ (bool true) (int 1) (int 2);
      Some 22, if_ (int 1 -. int 1 >. int 1) (int 1 +. int 10) (int 2 +. int 20);
      Some 11, if_ (int 2 -. int 1 <. int 2) (int 1 +. int 10) (int 2 +. int 20);
      Some 1, if_ (bool true) (int 1) (int 999);

      None, if_ (int 0 >. (loop (break @@ int 1))) (int 5) (int 3 +. int 2);
      None, if_ (int 0 >. (loop @@ int 0)) (int 1) (int 1);

      None, let_ "foo" (int 99) (get "foo");
      None,
      let_ "foo" (int (-1))
        (get "foo" +.
         let_ "foo" (int 24)
           (int 100 +. get "foo"));

      None,
      let_ "sum" (int 0)
        (loop
           (set "sum" (loop_index() *. loop_index() +. get "sum")
              (if_ (get "sum" >. int 10)
                 (break (get "sum"))
                 (int 1))))
    ]
end

let test_calc_optimized() =
  print_endline "Testing Algo_bindings optimization";
  let module S = Tests_algo_optimize(Algo_bindings.To_string) in
  let module O = Tests_algo_optimize(Algo_bindings.Optimize(Algo_bindings.To_string)) in
  let print (_, unoptimized) (expect, (info, optimized)) =
    let result_str = optimized ^ ", " ^ Compiler.Static_value.to_string info in
    match expect, Compiler.Static_value.is_known info with
    | Some _, true
    | None, false ->
      Tester_stats.ok unoptimized result_str
    | None, true ->
      Tester_stats.fail unoptimized "not to infer value" result_str
    | Some _, false ->
      Tester_stats.fail unoptimized "to infer value" optimized
      (* Printf.printf "  err-opt %s to %s, failed to infer value\n" unoptimized optimized *)
  in
  List.iter2 print S.bool_tests O.bool_tests;
  List.iter2 print S.int_tests O.int_tests

let () =
  test_bool ();
  test_int ();
  test_combined ();
  test_algo ();
  test_algo_bool ();
  test_algo_bindings ();
  test_algo_compiled ();
  test_calc_optimized ();
  Experimental.test ();
  Tester.finish ()



module type Test_cases =
sig
  type t
  val tests : t list
end

module type Test_suite =
sig
  include Test_cases
  val run : t -> unit
end

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
    if String.length testcase > 0 || String.length result > 0 then
      Printf.printf "  ok %s => %s\n" testcase result

  let finish () =
    if ((!errors) > 0) then begin
      Printf.printf "%d tests run, %d tests failed" (!total) (!errors);
      false
    end else begin
      Printf.printf "%d tests run\n" (!total);
      true
    end

  let suites : (string * (module Test_suite)) list ref = ref []

  let add name suite =
    suites := (name, suite) :: !suites

  let run () =
    let run_suite (name, suite) =
      Printf.printf "Test suite %s\n" name;
      let module S = (val suite : Test_suite) in
      List.iter S.run S.tests
    in
    List.iter run_suite !suites
end

module Tester_f(I : Interpreter.Create) : sig
  val run :
    string ->
    I.value option ->
    (I.t -> I.value) ->
    (I.value -> string) ->
    unit
end = struct
  include Tester_stats

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

module Tester_legacy = Tester_f(Interpreter.No_runtime)

type dyn_value = Interpreter.Default_values.value

module type Test_names =
sig
  type t = string
  type value = dyn_value

  val tests : (value option * string) list
end

module type Test_cases_eval =
sig
  module I : Interpreter.Create

  type t = I.t -> I.value
  val tests : (I.value option * t) list
end

let combine input testcases =
  List.map2
    (fun (_, name) (expected, testcase) -> name, expected, testcase)
    input
    testcases

module Test_runner_eval(C : Test_names)(E : Test_cases_eval with type I.value = C.value) =
struct
  type t = (string * C.value option * E.t)

  let tests : t list = combine C.tests E.tests

  let run (string, expected, f) =
    let module Tester = Tester_f(E.I) in
    Tester.run string expected f E.I.value_string

  let run_all case_name =
    Printf.printf "Testing %s\n" case_name;
    List.iter run tests
end

module Tests_int(L : Calc_int.Lang) =
struct
  type t = L.t
  type value = Interpreter.Default_values.value
  module I = Interpreter.No_runtime

  let is_int n = Some (I.int n)

  let tests = L.[
      is_int 3, int 1 +. int 2;
      is_int 1, int 1;
      is_int 99, int 99;
      is_int 106, int 100 +. int 3 *. int 2;
    ]
end

let add_test_int() =
  let module T : Test_suite =
    Test_runner_eval
      (Tests_int(Calc_int.To_string))
      (Tests_int(Calc_int.Eval(Interpreter.No_runtime)))
  in
  Tester_stats.add "Calc_int" (module T)

module Tests_bool(L : Calc_bool.Lang) =
struct
  type t = L.t
  module I = Interpreter.No_runtime
  type value = Interpreter.Default_values.value

  let is_bool b = Some (I.bool b)

  let tests = L.[
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
end

let add_test_bool () =
  let module T =
    Test_runner_eval
      (Tests_bool(Calc_bool.To_string))
      (Tests_bool(Calc_bool.Eval(Interpreter.No_runtime)))
  in
  Tester_stats.add "Calc_bool" (module T)

module Tests_combined(L : Calc.Lang) =
struct
  type t = L.t
  module I = Interpreter.No_runtime
  type value = Interpreter.Default_values.value

  let is_int n = Some (I.int n)
  let is_bool b = Some (I.bool b)

  let tests =
    let module Ci = Tests_int(L) in
    Ci.tests @ L.[
        is_int 1, int 1;
        is_int 15, int 10 +. int 5;
        is_int 123, (int 1 *. int 10 +. int 2) *. int 10 +. int 3;
        (* Some 5, int 10 / int 2; *)

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

let add_test_calc() =
  let module T =
    Test_runner_eval
      (Tests_combined(Calc.To_string))
      (Tests_combined(Calc.Eval(Interpreter.No_runtime)))
  in
  Tester_stats.add "Calc" (module T)

module Tests_algo(L : Algo_calc.Lang) =
struct
  type t = L.t
  module I = Interpreter.Dynamic
  type value = Interpreter.Default_values.value

  let is_int n = Some (I.int n)
  let is_bool b = Some (I.bool b)

  let tests =
    let module C = Tests_combined(L) in
    C.tests @ L.[
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

        is_bool true, bool true;
    ]
end

let add_test_algo() =
  let module T =
    Test_runner_eval
      (Tests_algo(Algo_calc.To_string))
      (Tests_algo(Algo_calc.Eval(Interpreter.Dynamic)))
  in
  Tester_stats.add "Algo_calc" (module T)

module Tests_algo_bool(L : Algo_bool.Lang) =
struct
  type t = L.t
  type value = Interpreter.Default_values.value
  module I = Interpreter.Dynamic

    let is_bool b = Some (I.bool b)

    let tests = L.[
        is_bool true, bool true;
        is_bool true, bool true || bool false;
        is_bool false, bool false || bool false;

        is_bool false,
        at (file "Tests_algo_bool.ml") ~line:10 ~column:8 @@
        bool true && bool false;
      ]
  end

let add_test_algo_bool() =
  let module T =
    Test_runner_eval
      (Tests_algo_bool(Algo_bool.To_string))
      (Tests_algo_bool(Algo_bool.Eval(Interpreter.Dynamic)))
  in
  Tester_stats.add "Algo_bool" (module T)

module Tests_algo_bindings(L : Algo_bindings.Lang) =
struct
  type t = L.t
  type value = Interpreter.Default_values.value
  module I = Interpreter.Dynamic

  let is_int n = Some (I.int n)
  let is_bool b = Some (I.bool b)

  let tests =
    let module C = Tests_algo(L) in
    C.tests @ L.[
        is_int 99, let_ "foo" (int 99) (get "foo");
        is_int 123,
        let_ "foo" (int (-1))
          (get "foo" +.
           let_ "foo" (int 24)
             (int 100 +. get "foo"));

        is_int 14,
        at (file "Tests_algo_bindings.ml") ~line:10 ~column:8 @@
        let_ "sum" (int 0)
          (loop
             (set "sum" (loop_index() *. loop_index() +. get "sum")
                (if_ (get "sum" >. int 10)
                   (break (get "sum"))
                   (int 1))))
      ]
end

let add_test_algo_bindings () =
  let module T =
    Test_runner_eval
      (Tests_algo_bindings(Algo_bindings.To_string))
      (Tests_algo_bindings(Algo_bindings.Eval(Interpreter.Dynamic)))
  in
  Tester_stats.add "Algo_bindings" (module T)

module Tests_algo_compiler_errors(L : Algo_bindings.Lang) =
struct
  type t = L.t
  type value = dyn_value

  let is_int n = Some (Interpreter.Default_values.int n)
  let is_bool b = Some (Interpreter.Default_values.bool b)

  let tests =
    let module B = Tests_algo_bindings(L) in
    B.tests @ L.[
      None, if_ (bool false) (loop_index()) (int 0);
      None,
      let_ "x" (int 10)
        (get "x" +. get "y")
    ]
end

module type Test_cases_compile =
sig
  type t = Compiler.Info.t * (Compiler.Context.t -> (Interpreter.No_runtime.t -> dyn_value) Compiler.Result.t)
  val tests : (dyn_value option * t) list
end

module Test_runner_compile(C : Test_names)(E : Test_cases_compile) : Test_suite =
struct
  type t = (string * C.value option * E.t)

  let tests : t list = combine C.tests E.tests

  let run (string, expected, (info, f)) =
    let check_and_run info compile ctx =
      let run =
        match compile @@ Compiler.Context.make () with
        | Result.Ok x -> x
        | Result.Error _ -> failwith "errors"
      in
      if false = Compiler.Info.validate info then
        failwith "invalid compiler info"
      else
        run ctx
    in
    let value_string = Interpreter.Default_values.value_string in
    Tester_legacy.run string expected (check_and_run info f) value_string
end

let add_test_algo_compiled () =
  print_endline "Testing Algo_calc compiled";
  let module P = Tests_algo_compiler_errors(Algo_bindings.To_string) in
  let module C = Tests_algo_compiler_errors(Algo_bindings.Eval_compiled) in
  let module T = Test_runner_compile(P)(C) in
  Tester_stats.add "Algo_bindings compiled" (module T)

module Tests_algo_optimize(L : Algo_bindings.Lang) =
struct
  type t = L.t

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
      Some 15,
      at (file "Tests_algo_optimize.ml") ~line:10 ~column:8 @@
      int 10 +. int 5;

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
      None, let_ "foo" (int 99) (int 1 +. get "foo");
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

let test_algo_optimized() =
  print_endline "Testing Algo_bindings optimization";
  let module S = Tests_algo_optimize(Algo_bindings.To_string) in
  let module Opt = Algo_bindings.Optimize(Algo_bindings.To_string) in
  let module O = Tests_algo_optimize(Opt) in
  let print (unoptimized, expect, (info, optimized)) =
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
  List.iter print (combine S.bool_tests O.bool_tests);
  List.iter print (combine S.int_tests O.int_tests)

let test_parser() =
  print_endline "Testing Algo_bindings parser";
  let module Opt = Algo_bindings.Optimize(Algo_bindings.To_string) in
  let module O = Tests_algo_optimize(Opt) in
  let module P = Parser.Parse(Algo_bindings.Parse_rules(Algo_bindings.Optimize(Algo_bindings.To_string))) in
  let check_parser (st, _expected, (orig_info, orig_opt)) =
    try
      let info, opt = P.parse st in
      if info = orig_info then
        Tester_stats.ok "" ""
      else
        Tester_stats.fail (Strlang.Tree.to_string st) "info1" "info2";
      if opt = orig_opt then
        Tester_stats.ok (Strlang.Tree.to_string st) opt
      else
        Tester_stats.fail (Strlang.Tree.to_string st) orig_opt opt
    with
    | Parser.Parse_error (msg, _) ->
      Tester_stats.fail (Strlang.Tree.to_string st) orig_opt ("parser error: " ^ msg)
    | _ ->
      Tester_stats.fail (Strlang.Tree.to_string st) orig_opt "exception"
  in
  let module St = Tests_algo_optimize(Algo_bindings.To_st(Strlang.Tree)) in
  List.iter check_parser (combine St.int_tests O.int_tests)

let () =
  try
    Experimental.test ();
    add_test_bool ();
    add_test_int ();
    add_test_calc ();
    add_test_algo ();
    add_test_algo_bool ();
    add_test_algo_bindings ();
    add_test_algo_compiled ();
    test_algo_optimized ();
    test_parser ();
    Tester_stats.run ();
    exit (if Tester_stats.finish () then 0 else 1);
  with _ as error ->
    Tester_stats.fail "" "" "exception during test run";
    ignore (Tester_stats.finish ());
    raise error


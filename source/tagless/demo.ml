
module Tester : sig
  val finish : unit -> unit
  val fail : string -> string -> string -> unit
  val ok : string -> string -> unit
  val run :
    string ->
    'a option ->
    (Interpreter_context.t -> 'a) ->
    ('a -> string) ->
    unit
end = struct
  let errors = ref 0
  let total = ref 0

  let finish () =
    if ((!errors) > 0) then begin
      Printf.printf "%d tests run, %d tests failed" (!total) (!errors);
      exit 1
    end else
      Printf.printf "%d tests run\n" (!total)

  let fail testcase expected result =
    incr total;
    incr errors;
    Printf.printf "  err %s => %s, expected %s\n" testcase result expected

  let ok testcase result =
    incr total;
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
end

module type Test_names =
sig
  type 'a t = string
  val int_tests : (int option * string) list
  val bool_tests : (bool option * string) list
end

module type Test_cases =
sig
  type 'a t = Interpreter_context.t -> 'a
  val int_tests : (int option * int t) list
  val bool_tests : (bool option * bool t) list
end

module Test_runner(C : Test_names)(E : Test_cases) =
struct
  let run case_name =
    Printf.printf "Testing %s\n" case_name;
    List.iter2 (fun (expected, string) (_, f) ->
        Tester.run string expected f string_of_int)
      C.int_tests E.int_tests;
    List.iter2 (fun (expected, string) (_, f) ->
        Tester.run string expected f string_of_bool)
      C.bool_tests E.bool_tests
end

module Tests_int(L : Calc_int_layer.Lang) =
struct
  type 'a t = 'a L.t

  let int_tests = L.[
      Some 3, int 1 +. int 2;
      Some 1, int 1;
      Some 99, int 99;
      Some 106, int 100 +. int 3 *. int 2;
    ]

  let bool_tests = []
end

let test_int() =
  let module T =
    Test_runner
      (Tests_int(Calc_int.To_string))
      (Tests_int(Calc_int.Eval))
  in
  T.run "Calc_int"

module Tests_bool(L : Calc_bool_layer.Lang) =
struct
  type 'a t = 'a L.t

  let bool_tests = L.[
      Some true, bool true;
      Some false, bool false;
      Some true, (bool true && bool true);
      Some false, (bool true && bool false);
      Some true, (bool true || bool false);
      Some true, (bool false || bool true);
      Some false, (bool false || bool false);
      Some true, (bool true && (bool true || bool false));
      Some false, (bool true && bool false || bool false && bool true);
    ]

  let int_tests = []
end

let test_bool () =
  let module T =
    Test_runner
      (Tests_bool(Calc_bool.To_string))
      (Tests_bool(Calc_bool.Eval))
  in
  T.run "Calc_bool"

module Tests_combined(L : Calc_layer.Lang) =
struct
  type 'a t = 'a L.t

  let int_tests =
    let module Ci = Tests_int(L) in
    Ci.int_tests @ L.[
      Some 1, int 1;
      Some 15, int 10 +. int 5;
      Some 123, (int 1 *. int 10 +. int 2) *. int 10 +. int 3;
      (* Some 5, int 10 / int 2; *)
    ]

  let bool_tests =
    let module Cb = Tests_bool(L) in
    Cb.bool_tests @ L.[
      Some true, bool true;
      Some true, int 4 <. int 10;
      Some false, int 4 >. int 10;
      Some true, int 3 =. int 3;
      Some false, int 3 =. int 4;
      Some true, int 3 >. int (-10);
      Some false, int 3 >. int 3;

      Some true, bool true || bool false;
      Some false, bool false || bool false;
      Some false, bool true && bool false;
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
      (Tests_combined(Calc.Eval))
  in
  T.run "Calc"

module Tests_algo(L : Algo_calc.Lang) =
struct
  type 'a t = 'a L.t

  let int_tests =
    let module C = Tests_combined(L) in
    C.int_tests @ L.[
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
    C.bool_tests @ L.[
      Some true, bool true;
      (* true, int 1 =. loop (int 1); *)
    ]
end

let test_algo() =
  let module T =
    Test_runner
      (Tests_algo(Algo_calc.To_string))
      (Tests_algo(Algo_calc.Eval))
  in
  T.run "Algo_calc"

module Tests_algo_bool(L : Algo_bool.Lang) =
struct
  type 'a t = 'a L.t

  let bool_tests =
    L.[
        Some true, bool true;
        Some true, bool true || bool false;
        Some false, bool false || bool false;
        Some false, bool true && bool false;
      ]

  let int_tests = []
end

let test_algo_bool() =
  let module T =
    Test_runner
      (Tests_algo_bool(Algo_bool.To_string))
      (Tests_algo_bool(Algo_bool.Eval))
  in
  T.run "Algo_bool"

module Tests_algo_bindings(L : Algo_bindings.Lang) =
struct
  type 'a t = 'a L.t

  let bool_tests = []

  let int_tests = L.[
      Some 99, let_ "foo" (int 99) (get "foo");
      Some 123,
      let_ "foo" (int (-1))
        (get "foo" +.
         let_ "foo" (int 24)
           (int 100 +. get "foo"));

      Some 14,
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
      (Tests_algo_bindings(Algo_bindings.Eval))
  in
  T.run "Algo_bindings"

module Tests_algo_compiled(L : Algo_calc.Lang) =
struct
  type 'a t = 'a L.t

  let bool_tests =
    let module T = Tests_algo(L) in
    T.bool_tests @ []

  let int_tests =
    let module T = Tests_algo(L) in
    T.int_tests @ L.[
      None, if_ (bool false) (loop_index()) (int 0);
    ]
end

let test_algo_compiled () =
  print_endline "Testing Algo_calc compiled";
  let module P = Tests_algo_compiled(Algo_calc.To_string) in
  let module C = Tests_algo_compiled(Algo_calc.Eval_compiled) in
  let check_and_run info f ctx =
    if false = Compiler_context.validate info then
      failwith "compiler error"
    else
      f ctx
  in
  List.iter2 (fun (expected, string) (_, (info, f)) ->
      Tester.run string expected (check_and_run info f) string_of_bool)
    P.bool_tests C.bool_tests;
  List.iter2 (fun (expected, string) (_, (info, f)) ->
      Tester.run string expected (check_and_run info f) string_of_int)
    P.int_tests C.int_tests

let () =
  test_bool ();
  test_int ();
  test_combined ();
  test_algo ();
  test_algo_bool ();
  test_algo_bindings ();
  test_algo_compiled();
  Tester.finish ()


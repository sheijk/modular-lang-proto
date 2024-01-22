
open Demo_tester

let add_test_int() =
  let module T : Test_suite =
    Demo_runner.Eval
      (Calc_int.Tests(Calc_int.To_string))
      (Calc_int.Tests(Calc_int.Eval(Interpreter.Dynamic)))
      (Interpreter.Dynamic)
  in
  Tester_state.add "Calc_int" (module T)

module Eval2
    (I : Interpreter.Create)
    (Tests : functor (L : Calc_bool.Lang) (I : Interpreter.Create) -> Demo_runner.Test_cases
     with type t = L.t
      and type interpreter = I.t
      and type value = I.value
      and type expected = I.value option)
= struct
  include Demo_runner.Eval
      (Tests(Calc_bool.To_string) (I))
      (Tests(Calc_bool.Eval(I)) (I))
      (I)
end

let add_test_bool () =
  let module T =
    Eval2 (Interpreter.No_runtime) (Calc_bool.Tests)
  in
  Tester_state.add "Calc_bool" (module T)

let add_test_calc() =
  let module T =
    Demo_runner.Eval
      (Calc.Tests(Calc.To_string))
      (Calc.Tests(Calc.Eval(Interpreter.Dynamic)))
      (Interpreter.Dynamic)
  in
  Tester_state.add "Calc" (module T)

let add_test_algo() =
  let module T =
    Demo_runner.Eval
      (Algo_calc.Tests(Algo_calc.To_string))
      (Algo_calc.Tests(Algo_calc.Eval(Interpreter.Dynamic)))
      (Interpreter.Dynamic)
  in
  Tester_state.add "Algo_calc" (module T)

let add_test_algo_bool() =
  let module T =
    Demo_runner.Eval
      (Algo_bool.Tests(Algo_bool.To_string))
      (Algo_bool.Tests(Algo_bool.Eval(Interpreter.Dynamic)))
      (Interpreter.Dynamic)
  in
  Tester_state.add "Algo_bool" (module T)

let add_test_algo_bindings () =
  let module T =
    Demo_runner.Eval
      (Algo_bindings.Tests(Algo_bindings.To_string))
      (Algo_bindings.Tests(Algo_bindings.Eval(Interpreter.Dynamic)))
      (Interpreter.Dynamic)
  in
  Tester_state.add "Algo_bindings" (module T)

let add_test_algo_compiled () =
  let module P = Algo_bindings.Tests_compiler_errors(Algo_bindings.To_string) in
  let module C = Algo_bindings.Tests_compiler_errors(Algo_bindings.Eval_compiled) in
  let module T = Demo_runner.Compile(P)(C) in
  Tester_state.add "Algo_bindings compiled" (module T)

let add_test_algo_optimized() =
  let module S = Algo_bindings.Tests_optimize(Algo_bindings.To_string) in
  let module Opt = Algo_bindings.Optimize(Algo_bindings.To_string) in
  let module O = Algo_bindings.Tests_optimize(Opt) in
  let module T = Demo_runner.Optimize(S)(O) in
  Tester_state.add "Algo_bindings optimization" (module T)

let add_test_parser() =
  let module Opt = Algo_bindings.Optimize(Algo_bindings.To_string) in
  let module O = Algo_bindings.Tests_optimize(Opt) in
  let module P = Parser.Parse(Algo_bindings.Parse_rules(Opt)) in
  let module St = Algo_bindings.Tests_optimize(Algo_bindings.To_st(Strlang.Tree)) in
  let module T = Demo_runner.Parse(St)(O)(P) in
  Tester_state.add "Algo_bindings parser" (module T)

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
    add_test_algo_optimized ();
    add_test_parser ();
    Tester_state.run ();
    exit (if Tester_state.finish () then 0 else 1);
  with _ as error ->
    Tester_state.fail "" "" "exception during test run";
    ignore (Tester_state.finish ());
    raise error


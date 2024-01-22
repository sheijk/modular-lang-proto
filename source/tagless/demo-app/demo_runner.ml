
open Demo_tester

module type Eval_cases =
sig
  module I : Interpreter.Create

  type t = I.t -> I.value
  val tests : (I.value option * t) list
end

module Eval(C : Test_names)(E : Eval_cases with type I.value = C.value) =
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

module type Compile_cases =
sig
  type t = Compiler.Info.t * (Compiler.Context.t -> (Interpreter.No_runtime.t -> dyn_value) Compiler.Result.t)
  val tests : (dyn_value option * t) list
end

module Compile(C : Test_names)(E : Compile_cases) : Test_suite =
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
    let module Tester = Tester_f(Interpreter.No_runtime) in
    Tester.run string expected (check_and_run info f) value_string
end

module type Optimize_cases =
sig
  type t = Compiler.Static_value.t * string
  val tests : (dyn_value option * t) list
end

module Optimize(C : Test_names)(E : Optimize_cases) : Test_suite =
struct
  type t = (string * C.value option * E.t)

  let tests : t list = combine C.tests E.tests

  let run (unoptimized, expect, (info, optimized)) =
    let result_str = optimized ^ ", " ^ Compiler.Static_value.to_string info in
    match expect, Compiler.Static_value.is_known info with
    | Some _, true
    | None, false ->
      Tester_state.ok unoptimized result_str
    | None, true ->
      Tester_state.fail unoptimized "not to infer value" result_str
    | Some _, false ->
      Tester_state.fail unoptimized "to infer value" optimized
end

module type Parse_names =
sig
  type t = Strlang.Tree.t
  type value = dyn_value

  val tests : (value option * t) list
end

module type Parse_cases =
sig
  type t = Compiler.Static_value.t * string
  val tests : (dyn_value option * t) list
end

module Parse
    (C : Parse_names)
    (E : Parse_cases)
    (P : sig val parse : Strlang.Tree.t -> E.t end) : Test_suite =
struct
  type t = Strlang.Tree.t * dyn_value option * E.t

  let tests : t list = combine C.tests E.tests

  let run (st, _expected, (orig_info, orig_opt)) =
    try
      let info, opt = P.parse st in
      if info = orig_info then
        Tester_state.ok "" ""
      else
        Tester_state.fail (Strlang.Tree.to_string st) "info1" "info2";
      if opt = orig_opt then
        Tester_state.ok (Strlang.Tree.to_string st) opt
      else
        Tester_state.fail (Strlang.Tree.to_string st) orig_opt opt
    with
    | Parser.Parse_error (msg, _) ->
      Tester_state.fail (Strlang.Tree.to_string st) orig_opt ("parser error: " ^ msg)
    | _ ->
      Tester_state.fail (Strlang.Tree.to_string st) orig_opt "exception"
end

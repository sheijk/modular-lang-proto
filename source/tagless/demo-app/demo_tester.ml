module type Test_suite =
sig
  type t
  val tests : t list
  val run : t -> unit
end

module Tester_state =
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
    List.iter run_suite (List.rev !suites)
end

let combine input testcases =
  List.map2
    (fun (_, name) (expected, testcase) -> name, expected, testcase)
    input
    testcases

module Tester_f(I : Interpreter.Create) : sig
  val run :
    string ->
    I.value option ->
    (I.t -> I.value) ->
    (I.value -> string) ->
    unit
end = struct
  include Tester_state

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

type dyn_value = Interpreter.Default_values.value



module type Lang =
sig
  type t
  val bool : bool -> t
  val ( && ) : t -> t -> t
  val ( || ) : t -> t -> t
end

module To_string =
struct
  type t = string
  let bool b = if b then "true" else "false"
  let ( && ) lhs rhs = Printf.sprintf "(%s and %s)" lhs rhs
  let ( || ) lhs rhs = Printf.sprintf "(%s or %s)" lhs rhs
end
let () = let module T : Lang = To_string in ()

module Eval =
struct
  type t = unit -> bool
  let bool b = fun () -> b
  let ( && ) lhs rhs = fun () -> ((lhs()) && (rhs()))
  let ( || ) lhs rhs = fun () -> ((lhs()) || (rhs()))
end
let () = let module T : Lang = Eval in ()

module Tests(L : Lang) =
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

let test() =
  let module C = Tests(To_string) in
  let module E = Tests(Eval) in
  List.iter2 (fun (expected, string) (_, f) ->
      let result = f() in
      if expected = result then
        Printf.printf "ok  %s => %b\n" string result
      else
        Printf.printf "err %s => %b, expected %b\n" string result expected)
    C.tests E.tests

let () =
  print_endline "hello, tagless Calc_bool";
  test ()


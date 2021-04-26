
module type Lang =
  sig
  type t
  val int : int -> t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
end

module To_string =
struct
  type t = string
  let int = string_of_int
  let ( + ) lhs rhs = Printf.sprintf "(%s + %s)" lhs rhs
  let ( - ) lhs rhs = Printf.sprintf "(%s - %s)" lhs rhs
  let ( * ) lhs rhs = Printf.sprintf "(%s * %s)" lhs rhs
  let ( / ) lhs rhs = Printf.sprintf "(%s / %s)" lhs rhs
end
let () = let module T : Lang = To_string in ()

module Eval =
struct
  type t = unit -> int
  let int n = fun () -> n
  let ( + ) lhs rhs = fun () -> (lhs()) + (rhs())
  let ( - ) lhs rhs = fun () -> (lhs()) - (rhs())
  let ( * ) lhs rhs = fun () -> (lhs()) * (rhs())
  let ( / ) lhs rhs = fun () -> (lhs()) / (rhs())
end
let () = let module T : Lang = Eval in ()

module Tests(L : Lang) =
struct
  let tests = L.[
      3, int 1 + int 2;
      1, int 1;
      99, int 99;
      106, int 100 + int 3 * int 2;
      10, int 10 + int 9999;
    ]
end

let test() =
  let module C = Tests(To_string) in
  let module E = Tests(Eval) in
  List.iter2 (fun (expected, string) (_, f) ->
      let result = f() in
      if expected = result then
        Printf.printf "ok  %s => %d\n" string result
      else
        Printf.printf "err %s => %d, expected %d\n" string result expected)
    C.tests E.tests

let () =
  print_endline "hello, tagless Calc_int";
  test ()


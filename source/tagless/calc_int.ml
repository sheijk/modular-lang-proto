
module type Lang =
  sig
  type t
  val int : int -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
end

module To_string =
struct
  type t = string
  let int = string_of_int
  let add lhs rhs = Printf.sprintf "(%s + %s)" lhs rhs
  let sub lhs rhs = Printf.sprintf "(%s - %s)" lhs rhs
  let mul lhs rhs = Printf.sprintf "(%s * %s)" lhs rhs
  let div lhs rhs = Printf.sprintf "(%s / %s)" lhs rhs
end
let () = let module T : Lang = To_string in ()

module Eval =
struct
  type t = unit -> int
  let int n = fun () -> n
  let add lhs rhs = fun () -> (lhs()) + (rhs())
  let sub lhs rhs = fun () -> (lhs()) - (rhs())
  let mul lhs rhs = fun () -> (lhs()) * (rhs())
  let div lhs rhs = fun () -> (lhs()) / (rhs())
end
let () = let module T : Lang = Eval in ()

module Tests(L : Lang) =
struct
  let tests = [(3, L.(add (int 1) (int 2)))]
end

let test() =
  let module C = Tests(To_string) in
  let module E = Tests(Eval) in
  List.iter2 (fun (expected, string) (_, f) ->
      let result = f() in
      if expected = result then
        Printf.printf "ok  %s => %d" string result
      else
        Printf.printf "err %s => %d, expected %d" string result expected)
    C.tests E.tests

let () =
  print_endline "hello, tagless Calc_int";
  test ()


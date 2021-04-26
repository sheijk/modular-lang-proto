
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


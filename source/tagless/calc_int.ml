
module type Lang =
sig
  type 'a t
  val int : int -> int t
  val ( + ) : int t -> int t -> int t
  val ( - ) : int t -> int t -> int t
  val ( * ) : int t -> int t -> int t
  val ( / ) : int t -> int t -> int t
end

module To_string'(T : sig type 'a t = string end) =
struct
  let int : int -> int T.t = string_of_int
  let ( + ) lhs rhs = Printf.sprintf "(%s + %s)" lhs rhs
  let ( - ) lhs rhs = Printf.sprintf "(%s - %s)" lhs rhs
  let ( * ) lhs rhs = Printf.sprintf "(%s * %s)" lhs rhs
  let ( / ) lhs rhs = Printf.sprintf "(%s / %s)" lhs rhs
end

module To_string = struct
  type 'a t = string
  include To_string'(struct type 'a t = string end)
end
let () = let module T : Lang = To_string in ()

module Eval'(T : sig type 'a t = unit -> 'a end) =
struct
  let int n : int T.t = fun () -> n
  let ( + ) lhs rhs = fun () -> (lhs()) + (rhs())
  let ( - ) lhs rhs = fun () -> (lhs()) - (rhs())
  let ( * ) lhs rhs = fun () -> (lhs()) * (rhs())
  let ( / ) lhs rhs = fun () -> (lhs()) / (rhs())
end

module Eval =
struct
  type 'a t = unit -> 'a
  include Eval'(struct type 'a t = unit -> 'a end)
end
let () = let module T : Lang = Eval in ()



module type Lang =
  sig
  type t
  val int : int -> t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
end

module To_string'(T : sig type t = string end) =
struct
  let int : int -> T.t = string_of_int
  let ( + ) lhs rhs = Printf.sprintf "(%s + %s)" lhs rhs
  let ( - ) lhs rhs = Printf.sprintf "(%s - %s)" lhs rhs
  let ( * ) lhs rhs = Printf.sprintf "(%s * %s)" lhs rhs
  let ( / ) lhs rhs = Printf.sprintf "(%s / %s)" lhs rhs
end

module To_string = struct
  type t = string
  include To_string'(struct type t = string end)
end
let () = let module T : Lang = To_string in ()

module Eval'(T : sig type t = unit -> int end) =
struct
  let int n : T.t = fun () -> n
  let ( + ) lhs rhs = fun () -> (lhs()) + (rhs())
  let ( - ) lhs rhs = fun () -> (lhs()) - (rhs())
  let ( * ) lhs rhs = fun () -> (lhs()) * (rhs())
  let ( / ) lhs rhs = fun () -> (lhs()) / (rhs())
end

module Eval =
struct
  type t = unit -> int
  include Eval'(struct type t = unit -> int end)
end
let () = let module T : Lang = Eval in ()


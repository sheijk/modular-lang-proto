
module type Lang =
sig
  type t
  val bool : bool -> t
  val ( && ) : t -> t -> t
  val ( || ) : t -> t -> t
end

module To_string'(T : sig type t = string end) =
struct
  let bool b : T.t = if b then "true" else "false"
  let ( && ) lhs rhs = Printf.sprintf "(%s and %s)" lhs rhs
  let ( || ) lhs rhs = Printf.sprintf "(%s or %s)" lhs rhs
end

module To_string = struct
  type t = string
  include To_string'(struct type t = string end)
end
let () = let module T : Lang = To_string in ()

module Eval'(T : sig type t = unit -> bool end) =
struct
  let bool b : T.t = fun () -> b
  let ( && ) lhs rhs = fun () -> ((lhs()) && (rhs()))
  let ( || ) lhs rhs = fun () -> ((lhs()) || (rhs()))
end

module Eval =
struct
  type t = unit -> bool
  include Eval'(struct type t = unit -> bool end)
end
let () = let module T : Lang = Eval in ()



module type Lang =
sig
  type 'a t
  val bool : bool -> bool t
  val ( && ) : bool t -> bool t -> bool t
  val ( || ) : bool t -> bool t -> bool t
end

module To_string'(T : sig type 'a t = string end) =
struct
  let bool b : bool T.t = if b then "true" else "false"
  let ( && ) lhs rhs = Printf.sprintf "(%s and %s)" lhs rhs
  let ( || ) lhs rhs = Printf.sprintf "(%s or %s)" lhs rhs
end

module To_string = struct
  type 'a t = string
  include To_string'(struct type 'a t = string end)
end
let () = let module T : Lang = To_string in ()

module Eval'(T : sig type 'a t = unit -> 'a end) =
struct
  let bool b : bool T.t = fun () -> b
  let ( && ) lhs rhs = fun () -> ((lhs()) && (rhs()))
  let ( || ) lhs rhs = fun () -> ((lhs()) || (rhs()))
end

module Eval =
struct
  type 'a t = unit -> 'a
  include Eval'(struct type 'a t = unit -> 'a end)
end
let () = let module T : Lang = Eval in ()


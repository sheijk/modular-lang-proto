
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


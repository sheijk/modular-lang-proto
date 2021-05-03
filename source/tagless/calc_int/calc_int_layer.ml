module type Lang =
sig
  type 'a t
  val int : int -> int t
  val ( +. ) : int t -> int t -> int t
  val ( -. ) : int t -> int t -> int t
  val ( *. ) : int t -> int t -> int t
  val ( /. ) : int t -> int t -> int t
end

module To_string =
struct
  let int = string_of_int
  let ( +. ) lhs rhs = Printf.sprintf "(%s +. %s)" lhs rhs
  let ( -. ) lhs rhs = Printf.sprintf "(%s -. %s)" lhs rhs
  let ( *. ) lhs rhs = Printf.sprintf "(%s *. %s)" lhs rhs
  let ( /. ) lhs rhs = Printf.sprintf "(%s /. %s)" lhs rhs
end

module Eval =
struct
  let int n = fun _ -> n
  let ( +. ) lhs rhs = fun ctx -> (lhs ctx) + (rhs ctx)
  let ( -. ) lhs rhs = fun ctx -> (lhs ctx) - (rhs ctx)
  let ( *. ) lhs rhs = fun ctx -> (lhs ctx) * (rhs ctx)
  let ( /. ) lhs rhs = fun ctx -> (lhs ctx) / (rhs ctx)
end

let apply f (lhs_info, lhs) (rhs_info, rhs) =
  Compiler_context.merge lhs_info rhs_info,
  fun ctx ->
    (f (lhs ctx) (rhs ctx))

module Eval_compiled =
struct
  include Empty.Eval_compiled

  let int i = Compiler_context.make(), fun _ -> i

  let ( +. ) = apply ( + )
  let ( -. ) = apply ( - )
  let ( *. ) = apply ( * )
  let ( /. ) = apply ( / )
end
let () = let module T : Lang = Eval_compiled in ()


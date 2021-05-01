
module Layer =
struct
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
end

module Full =
struct
  module type Lang = Layer.Lang

  module To_string = struct
    type 'a t = string
    include Layer.To_string
  end
  let () = let module T : Lang = To_string in ()

  module Eval =
  struct
    include Empty.Eval
    include Layer.Eval
  end
  let () = let module T : Lang = Eval in ()
end

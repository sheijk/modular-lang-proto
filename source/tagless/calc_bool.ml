
module Layer =
struct
  module type Lang =
  sig
    type 'a t
    val bool : bool -> bool t
    val ( && ) : bool t -> bool t -> bool t
    val ( || ) : bool t -> bool t -> bool t
  end

  module To_string =
  struct
    let bool b = if b then "true" else "false"
    let ( && ) lhs rhs = Printf.sprintf "(%s && %s)" lhs rhs
    let ( || ) lhs rhs = Printf.sprintf "(%s || %s)" lhs rhs
  end

  module Eval =
  struct
    let bool b = fun _ -> b
    let ( && ) lhs rhs = fun ctx -> ((lhs ctx) && (rhs ctx))
    let ( || ) lhs rhs = fun ctx -> ((lhs ctx) || (rhs ctx))
  end
end

module Full =
struct
  module type Lang =
  sig
    include Layer.Lang
  end

  module To_string =
  struct
    type 'a t = string
    include Layer.To_string
  end
  let () = let module T : Lang = To_string in ()

  module Eval =
  struct
    include Eval_base.T
    include Layer.Eval
  end
  let () = let module T : Lang = Eval in ()
end

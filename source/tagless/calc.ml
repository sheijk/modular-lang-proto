
module Layer =
struct
  module type Lang =
  sig
    type 'a t
    include Calc_int.Layer.Lang with type 'a t := 'a t
    include Calc_bool.Layer.Lang with type 'a t := 'a t

    val int_to_float : int t -> float t
    val float_to_int : float t -> int t
    val ( =. ) : int t -> int t -> bool t
    val ( <. ) : int t -> int t -> bool t
    val ( >. ) : int t -> int t -> bool t
  end

  module To_string =
  struct
    include Calc_int.Layer.To_string
    include Calc_bool.Layer.To_string

    let int_to_float = Printf.sprintf "(int_to_float %s)"
    let float_to_int = Printf.sprintf "(float_to_int %s)"
    let ( =. ) = Printf.sprintf "(%s =. %s)"
    let ( <. ) = Printf.sprintf "(%s <. %s)"
    let ( >. ) = Printf.sprintf "(%s >. %s)"
  end

  module Eval =
  struct
    include Calc_bool.Layer.Eval
    include Calc_int.Layer.Eval

    let int_to_float value =
      fun ctx -> float_of_int (value ctx)

    let float_to_int value =
      fun ctx -> int_of_float (value ctx)

    let ( =. ) lhs rhs = fun ctx -> (lhs ctx) = (rhs ctx)
    let ( <. ) lhs rhs = fun ctx -> (lhs ctx) < (rhs ctx)
    let ( >. ) lhs rhs = fun ctx -> (lhs ctx) > (rhs ctx)
  end
end

module Full =
struct
  module type Lang = Layer.Lang

  module To_string =
  struct
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

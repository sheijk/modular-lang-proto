module type Lang =
sig
  type 'a t
  include Calc_int_layer.Lang with type 'a t := 'a t
  include Calc_bool_layer.Lang with type 'a t := 'a t

  val int_to_float : int t -> float t
  val float_to_int : float t -> int t
  val ( =. ) : int t -> int t -> bool t
  val ( <. ) : int t -> int t -> bool t
  val ( >. ) : int t -> int t -> bool t
end

module To_string =
struct
  include Calc_int_layer.To_string
  include Calc_bool_layer.To_string

  let int_to_float = Printf.sprintf "(int_to_float %s)"
  let float_to_int = Printf.sprintf "(float_to_int %s)"
  let ( =. ) = Printf.sprintf "(%s =. %s)"
  let ( <. ) = Printf.sprintf "(%s <. %s)"
  let ( >. ) = Printf.sprintf "(%s >. %s)"
end

module Eval =
struct
  include Calc_bool_layer.Eval
  include Calc_int_layer.Eval

  let int_to_float value =
    fun ctx -> float_of_int (value ctx)

  let float_to_int value =
    fun ctx -> int_of_float (value ctx)

  let ( =. ) lhs rhs = fun ctx -> (lhs ctx) = (rhs ctx)
  let ( <. ) lhs rhs = fun ctx -> (lhs ctx) < (rhs ctx)
  let ( >. ) lhs rhs = fun ctx -> (lhs ctx) > (rhs ctx)
end

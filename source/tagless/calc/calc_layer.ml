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

let apply f (lhs_info, lhs) (rhs_info, rhs) =
  Compiler_context.merge lhs_info rhs_info,
  fun ctx ->
    (f (lhs ctx) (rhs ctx))

module Eval_compiled =
struct
  include Empty.Eval_compiled
  include Calc_bool_layer.Eval_compiled
  include Calc_int_layer.Eval_compiled

  let int_to_float (context, value) = context, fun ctx -> float_of_int (value ctx)
  let float_to_int (context, value) = context, fun ctx -> int_of_float (value ctx)

  let ( =. ) = apply ( = )
  let ( <. ) = apply ( < )
  let ( >. ) = apply ( > )
end
let () = let module T : Lang = Eval_compiled in ()


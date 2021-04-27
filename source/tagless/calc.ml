
module type Lang =
sig
  type 'a t
  include Calc_int.Lang with type 'a t := 'a t
  include Calc_bool.Lang with type 'a t := 'a t

  val int_to_float : int t -> float t
  val float_to_int : float t -> int t
  val ( =. ) : int t -> int t -> bool t
  val ( <. ) : int t -> int t -> bool t
  val ( >. ) : int t -> int t -> bool t
end

module To_string =
struct
  type 'a t = string
  include Calc_int.To_string'(struct type 'a t = string end)
  include Calc_bool.To_string'(struct type 'a t = string end)

  let int_to_float = Printf.sprintf "(int_to_float %s)"
  let float_to_int = Printf.sprintf "(float_to_int %s)"
  let ( =. ) = Printf.sprintf "(%s =. %s)"
  let ( <. ) = Printf.sprintf "(%s <. %s)"
  let ( >. ) = Printf.sprintf "(%s >. %s)"
end
let () = let module T : Lang = To_string in ()

module Eval =
struct
  include Eval_base.T

  include Calc_bool.Eval'(Eval_base.T)
  include Calc_int.Eval'(Eval_base.T)

  let int_to_float value =
    fun ctx -> float_of_int (value ctx)

  let float_to_int value =
    fun ctx -> int_of_float (value ctx)

  let ( =. ) lhs rhs = fun ctx -> (lhs ctx) = (rhs ctx)
  let ( <. ) lhs rhs = fun ctx -> (lhs ctx) < (rhs ctx)
  let ( >. ) lhs rhs = fun ctx -> (lhs ctx) > (rhs ctx)
end
let () = let module T : Lang = Eval in ()

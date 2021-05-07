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
let () = let module T : Lang = To_string in ()

module Eval(I : Interpreter.Empty) =
struct
  include Empty.Eval(I)
  include Calc_bool_layer.Eval(I)
  include Calc_int_layer.Eval(I)

  let int_to_float value =
    fun ctx -> float_of_int (value ctx)

  let float_to_int value =
    fun ctx -> int_of_float (value ctx)

  let ( =. ) lhs rhs = fun ctx -> (lhs ctx) = (rhs ctx)
  let ( <. ) lhs rhs = fun ctx -> (lhs ctx) < (rhs ctx)
  let ( >. ) lhs rhs = fun ctx -> (lhs ctx) > (rhs ctx)
end
let () = let module T : Lang = Eval(Interpreter.No_runtime) in ()

let apply f (lhs_info, lhs) (rhs_info, rhs) =
  Compiler.Info.merge lhs_info rhs_info,
  fun ctx ->
    let lhs = lhs ctx
    and rhs = rhs ctx
    in
    fun ctx ->
      (f (lhs ctx) (rhs ctx))

module Eval_compiled =
struct
  include Empty.Eval_compiled
  include Calc_bool_layer.Eval_compiled
  include Calc_int_layer.Eval_compiled

  let int_to_float (context, value) =
    context,
    fun ctx ->
      let value = value ctx in
      fun ctx -> float_of_int (value ctx)

  let float_to_int (context, value) =
    context,
    fun ctx ->
      let value = value ctx in
      fun ctx -> int_of_float (value ctx)

  let ( =. ) = apply ( = )
  let ( <. ) = apply ( < )
  let ( >. ) = apply ( > )
end
let () = let module T : Lang = Eval_compiled in ()

let add_plus_one lhs rhs = lhs + rhs + 1

module Count_ast_size =
struct
  include Empty.Count_ast_size
  include Calc_bool_layer.Count_ast_size
  include Calc_int_layer.Count_ast_size

  let int_to_float value = value + 1
  let float_to_int value = value + 1

  let ( =. ) = add_plus_one
  let ( <. ) = add_plus_one
  let ( >. ) = add_plus_one
end
let () = let module T : Lang = Count_ast_size in ()

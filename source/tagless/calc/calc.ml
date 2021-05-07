module type Lang =
sig
  type 'a t
  include Calc_int.Lang with type 'a t := 'a t
  include Calc_bool.Lang with type 'a t := 'a t

  val ( =. ) : int t -> int t -> bool t
  val ( <. ) : int t -> int t -> bool t
  val ( >. ) : int t -> int t -> bool t
end

module To_string =
struct
  include Calc_int.To_string
  include Calc_bool.To_string

  let ( =. ) = Printf.sprintf "(%s =. %s)"
  let ( <. ) = Printf.sprintf "(%s <. %s)"
  let ( >. ) = Printf.sprintf "(%s >. %s)"
end
let () = let module T : Lang = To_string in ()

module Eval(I : Interpreter.Empty) =
struct
  include Empty.Eval(I)
  include Calc_bool.Eval(I)
  include Calc_int.Eval(I)

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
  include Calc_bool.Eval_compiled
  include Calc_int.Eval_compiled

  let ( =. ) = apply ( = )
  let ( <. ) = apply ( < )
  let ( >. ) = apply ( > )
end
let () = let module T : Lang = Eval_compiled in ()

let add_plus_one lhs rhs = lhs + rhs + 1

module Count_ast_size =
struct
  include Empty.Count_ast_size
  include Calc_bool.Count_ast_size
  include Calc_int.Count_ast_size

  let ( =. ) = add_plus_one
  let ( <. ) = add_plus_one
  let ( >. ) = add_plus_one
end
let () = let module T : Lang = Count_ast_size in ()

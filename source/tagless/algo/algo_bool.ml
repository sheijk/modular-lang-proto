module type Lang = sig
  type 'a t
  include Algo_layer.Lang with type 'a t := 'a t
  include Calc_bool_layer.Lang with type 'a t := 'a t
end

module To_string = struct
  include Empty.To_string
  include Calc_bool_layer.To_string
  include Algo_layer.To_string
end
let () = let module T : Lang = To_string in ()

module Eval(I : Interpreter.Loop) = struct
  include Empty.Eval(I)
  include Calc_bool_layer.Eval(I)
  include Algo_layer.Eval(I)
end
let () = let module T : Lang = Eval(Interpreter.Dynamic) in ()

module Eval_compiled =
struct
  include Empty.Eval_compiled
  include Calc_bool_layer.Eval_compiled
  include Algo_layer.Eval_compiled
end
let () = let module T : Lang = Eval_compiled in ()


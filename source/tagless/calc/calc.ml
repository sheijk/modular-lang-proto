
module type Lang = Calc_layer.Lang

module To_string =
struct
  include Empty.To_string
  include Calc_layer.To_string
end
let () = let module T : Lang = To_string in ()

module Eval(I : Interpreter.Empty) =
struct
  include Empty.Eval(I)
  include Calc_layer.Eval(I)
end
let () = let module T : Lang = Eval(Interpreter.No_runtime) in ()

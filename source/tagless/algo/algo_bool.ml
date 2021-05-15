module type Lang = sig
  type t
  include Algo.Lang with type t := t
  include Calc_bool.Lang with type t := t
end

module To_string = struct
  include Empty.To_string
  include Calc_bool.To_string
  include Algo.To_string
end
let () = let module T : Lang = To_string in ()

module Eval(I : Interpreter.Loop) = struct
  include Empty.Eval(I)
  include Calc_bool.Eval(I)
  include Algo.Eval(I)
end
let () = let module T : Lang = Eval(Interpreter.Dynamic) in ()

module Eval_compiled =
struct
  include Empty.Eval_compiled
  include Calc_bool.Eval_compiled
  include Algo.Eval_compiled
end
let () = let module T : Lang = Eval_compiled in ()


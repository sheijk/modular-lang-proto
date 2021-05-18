module type Lang = sig
  type t
  include Algo.Lang with type t := t
  include Calc_bool.Lang with type t := t
  include Locations.Lang with type t := t
end

module To_st(S : Strlang.Lang) =
struct
  include Empty.To_st(S)
  include Calc_bool.To_st(S)
  include Algo.To_st(S)
  include Locations.To_st(S)
end
module To_string = To_st(Strlang.To_string)
let () = let module T : Lang = To_string in ()

module Eval(I : Interpreter.Loop) = struct
  include Empty.Eval(I)
  include Calc_bool.Eval(I)
  include Algo.Eval(I)
  include Locations.Eval(I)
end
let () = let module T : Lang = Eval(Interpreter.Dynamic) in ()

module Eval_compiled =
struct
  include Empty.Eval_compiled
  include Calc_bool.Eval_compiled
  include Algo.Eval_compiled
  include Locations.Eval_compiled
end
let () = let module T : Lang = Eval_compiled in ()


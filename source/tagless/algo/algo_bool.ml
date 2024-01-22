module type Lang = sig
  type t
  include Algo.Lang with type t := t
  include Calc_bool.Lang with type t := t
  include Locations.Lang with type t := t
end

module Tests(L : Lang) =
struct
  type t = L.t
  type value = Interpreter.Default_values.value
  module I = Interpreter.Dynamic

  let is_bool b = Some (I.bool b)

  let tests = L.[
      is_bool true, bool true;
      is_bool true, bool true || bool false;
      is_bool false, bool false || bool false;

      is_bool false,
      at (file "Algo_bool.Tests.ml") ~line:10 ~column:8 @@
      bool true && bool false;
    ]
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


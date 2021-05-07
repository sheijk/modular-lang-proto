module type Lang =
sig
  type 'a t
  include Calc.Lang with type 'a t := 'a t
  include Algo.Lang with type 'a t := 'a t
  include Bindings.Lang with type 'a t := 'a t
end

module To_string =
struct
  include Empty.To_string
  include Calc.To_string
  include Algo.To_string
  include Bindings.To_string
end
let () = let module T : Algo.Lang = To_string in ()

module Eval(I : Interpreter.All) =
struct
  include Empty.Eval(I)
  include Calc.Eval(I)
  include Algo.Eval(I)
  include Bindings.Eval(I)
end
let () = let module T : Algo.Lang = Eval(Interpreter.Dynamic) in ()

module Eval_compiled =
struct
  include Empty.Eval_compiled
  include Calc.Eval_compiled
  include Algo.Eval_compiled
  include Bindings.Eval_compiled
end
let () = let module T : Lang = Eval_compiled in ()

module Count_ast_size =
struct
  include Empty.Count_ast_size
  include Calc.Count_ast_size
  include Algo.Count_ast_size
  include Bindings.Count_ast_size
end
let () = let module T : Lang = Count_ast_size in ()

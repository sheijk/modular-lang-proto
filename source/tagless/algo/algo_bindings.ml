module type Lang =
sig
  type 'a t
  include Calc_layer.Lang with type 'a t := 'a t
  include Algo_layer.Lang with type 'a t := 'a t
  include Bindings_layer.Lang with type 'a t := 'a t
end

module To_string =
struct
  include Empty.To_string
  include Calc_layer.To_string
  include Algo_layer.To_string
  include Bindings_layer.To_string
end
let () = let module T : Algo_layer.Lang = To_string in ()

module Eval =
struct
  include Empty.Eval
  include Calc_layer.Eval
  include Algo_layer.Eval
  include Bindings_layer.Eval
end
let () = let module T : Algo_layer.Lang = Eval in ()

module Eval_compiled =
struct
  include Empty.Eval_compiled
  include Calc_layer.Eval_compiled
  include Algo_layer.Eval_compiled
  include Bindings_layer.Eval_compiled
end
let () = let module T : Lang = Eval_compiled in ()

module Count_ast_size =
struct
  include Empty.Count_ast_size
  include Calc_layer.Count_ast_size
  include Algo_layer.Count_ast_size
  include Bindings_layer.Count_ast_size
end
let () = let module T : Lang = Count_ast_size in ()

module type Lang =
sig
  type 'a t
  include Calc_layer.Lang with type 'a t := 'a t
  include Algo_layer.Lang with type 'a t := 'a t
end

module To_string =
struct
  type 'a t = string
  include Calc_layer.To_string
  include Algo_layer.To_string
end
let () = let module T : Algo_layer.Lang = To_string in ()

module Eval =
struct
  include Empty.Eval
  include Calc_layer.Eval
  include Algo_layer.Eval
end
let () = let module T : Algo_layer.Lang = Eval in ()

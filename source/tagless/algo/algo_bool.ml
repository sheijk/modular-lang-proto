module type Lang = sig
  type 'a t
  include Algo_layer.Lang with type 'a t := 'a t
  include Calc_bool_layer.Lang with type 'a t := 'a t
end

module To_string = struct
  type 'a t = string
  include Calc_bool_layer.To_string
  include Algo_layer.To_string
end

module Eval = struct
  include Empty.Eval
  include Calc_bool_layer.Eval
  include Algo_layer.Eval
end

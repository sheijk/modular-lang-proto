
module type Lang = Calc_bool_layer.Lang

module To_string =
struct
  type 'a t = string
  include Calc_bool_layer.To_string
end
let () = let module T : Lang = To_string in ()

module Eval =
struct
  include Empty.Eval
  include Calc_bool_layer.Eval
end
let () = let module T : Lang = Eval in ()


module type Lang =
sig
  type 'a t

  val if_ : bool t -> int t -> int t -> int t
  val loop : int t -> int t
  val break : int t -> int t
  val loop_index : unit -> int t
end

module To_string' =
struct
  let if_ condition true_ false_ =
    Printf.sprintf "(if %s then %s else %s)" condition true_ false_

  let loop body =
    Printf.sprintf "(loop %s)" body
  let break condition =
    Printf.sprintf "(break_if %s)" condition
  let loop_index () =
    Printf.sprintf "index"
end

module To_string =
struct
  include To_string'
  include Calc.To_string
end
let () = let module T : Lang = To_string in ()

module Eval' =
struct
  exception Loop_break of int

  let if_ condition true_ false_ = fun ctx ->
    if (condition ctx) then
      true_ ctx
    else
      false_ ctx

  let loop body = fun _ ->
    let rec loop index =
      if index > 100 then
        failwith "too many loop iterations";
      ignore (body { Eval_base.index = Some index });
      loop (index + 1)
    in
    try
      loop 0
    with Loop_break i ->
      i

  let break value = fun ctx ->
    raise (Loop_break (value ctx))

  let loop_index () = function
      | { Eval_base.index = Some index } ->
        index
      | { Eval_base.index = None } ->
        failwith "index used outside of loop"
end

module Eval =
struct
  include Calc.Eval
  include Eval'
end

let () = let module T : Lang = Eval in ()

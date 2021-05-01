module type Lang =
sig
  type 'a t

  val if_ : bool t -> int t -> int t -> int t
  val loop : int t -> int t
  val break : int t -> int t
  val loop_index : unit -> int t
end

module To_string =
struct
  let if_ condition true_ false_ =
    Printf.sprintf "(if %s then %s else %s)" condition true_ false_

  let loop body =
    Printf.sprintf "(loop %s)" body
  let break condition =
    Printf.sprintf "(break %s)" condition
  let loop_index () =
    Printf.sprintf "loop_index"
end

module Eval =
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
      ignore (body { Interpreter_context.index = Some index });
      loop (index + 1)
    in
    try
      loop 0
    with Loop_break i ->
      i

  let break value = fun ctx ->
    raise (Loop_break (value ctx))

  let loop_index () = function
    | { Interpreter_context.index = Some index } ->
      index
    | { Interpreter_context.index = None } ->
      failwith "index used outside of loop"
end
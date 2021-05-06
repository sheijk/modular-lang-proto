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

module Eval_generic(I : Interpreter.Loop) =
struct
  exception Loop_break of int

  let if_ condition true_ false_ = fun (ctx : I.t) ->
    if (condition ctx) then
      true_ ctx
    else
      false_ ctx

  let loop body = fun (ctx : I.t) ->
    let rec loop index =
      if index > 100 then
        failwith "too many loop iterations";
      ignore (body @@ I.with_index ctx index);
      loop (index + 1)
    in
    try
      loop 0
    with Loop_break i ->
      i

  let break value = fun ctx ->
    raise (Loop_break (value ctx))

  let loop_index () = fun (ctx : I.t) ->
    match I.loop_index ctx with
    | Some index ->
      index
    | None ->
      failwith "index used outside of loop"
end

module Eval = Eval_generic(Interpreter.Dynamic)

module Eval_compiled =
struct
  exception Loop_break of int

  let if_ (c_info, condition) (t_info, true_) (f_info, false_) =
    let info =
      Compiler.Info.merge
        c_info
        (Compiler.Info.merge f_info t_info)
    in
    info, fun (ctx : Compiler.Context.t) ->
      let condition = condition ctx
      and true_ = true_ ctx
      and false_ = false_ ctx
      in
      fun (ctx : Interpreter_context.t) ->
        if (condition ctx) then
          true_ ctx
        else
          false_ ctx

  let loop (b_info, body) =
    let _loop_num, info = Compiler.Info.mark_loop b_info in
    info, fun (ctx : Compiler.Context.t) ->
      let loop_index = ref 0 in
      let body = body @@ Compiler.Context.new_loop_index ctx loop_index in
      fun (ctx : Interpreter_context.t) ->
        let rec loop index =
          loop_index := index;
          if index > 100 then
            failwith "too many loop iterations";
          ignore (body ctx);
          loop (index + 1)
        in
        try
          loop 0
        with Loop_break i ->
          i

  let break (v_info, value) =
    v_info, fun ctx ->
      let value = value ctx in
      fun ctx ->
        raise (Loop_break (value ctx))

  let loop_index () =
    Compiler.Info.mark_loop_index @@ Compiler.Info.make(),
    function
    | { Compiler.Context.loop_index = Some index; _ } ->
      fun _ -> !index
    | { Compiler.Context.loop_index = None; _ } ->
      failwith "index used outside of loop"
end


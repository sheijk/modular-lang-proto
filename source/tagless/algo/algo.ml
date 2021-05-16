module type Lang =
sig
  type t

  val if_ : t -> t -> t -> t
  val loop : t -> t
  val break : t -> t
  val loop_index : unit -> t
end

module To_st(S : Strlang.Lang) =
struct
  include Empty.To_st(S)

  let if_ condition true_ false_ =
    S.tree [S.leaf "if"; condition; true_; false_]

  let loop body =
    S.tree [S.leaf "loop"; body]
  let break condition =
    S.tree [S.leaf "break"; condition]
  let loop_index () =
    S.leaf "loop_index"
end
module To_string = To_st(Strlang.To_string)
let () = let module T : Lang = To_string in ()

module Eval(I : Interpreter.Loop) =
struct
  include Empty.Eval(I)

  exception Loop_break of int

  let if_ condition true_ false_ = fun (ctx : I.t) ->
    if (I.match_bool ((=) true) (condition ctx)) then
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
      I.int i

  let break value = fun ctx ->
    I.match_int
      (fun i -> raise (Loop_break i))
      (value ctx)

  let loop_index () = fun (ctx : I.t) ->
    match I.loop_index ctx with
    | Some index ->
      I.int index
    | None ->
      failwith "index used outside of loop"
end
let () = let module T : Lang = Eval(Interpreter.Dynamic) in ()

module Eval_compiled =
struct
  include Empty.Eval_compiled
  module I = Interpreter.No_runtime

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
      fun ctx ->
        if (I.match_bool ((=) true) (condition ctx)) then
          true_ ctx
        else
          false_ ctx

  let loop (b_info, body) =
    let _loop_num, info = Compiler.Info.mark_loop b_info in
    info, fun (ctx : Compiler.Context.t) ->
      let loop_index = ref 0 in
      let body = body @@ Compiler.Context.new_loop_index ctx loop_index in
      fun ctx ->
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
          I.int i

  let break (v_info, value) =
    v_info, fun ctx ->
      let value = value ctx in
      fun ctx ->
        I.match_int
          (fun i -> raise (Loop_break i))
          (value ctx)

  let loop_index () =
    Compiler.Info.mark_loop_index @@ Compiler.Info.make(),
    function
    | { Compiler.Context.loop_index = Some index; _ } ->
      fun _ -> I.int (!index)
    | { Compiler.Context.loop_index = None; _ } ->
      failwith "index used outside of loop"
end
let () = let module T : Lang = Eval_compiled in ()

module Count_ast_size =
struct
  type t = int

  let if_ condition true_ false_ = condition + true_ + false_ + 1
  let loop body = body + 1
  let break value = value + 1
  let loop_index () = 1
end
(* let () = let module T : Lang = Count_ast_size in () *)

module Optimize(L : Lang) =
struct
  include Empty.Optimize(L)

  let loop (_, body) = Compiler.Static_value.unknown, L.loop body
  let loop_index () = Compiler.Static_value.unknown, L.loop_index ()
  let break (_, expr) = Compiler.Static_value.unknown, L.break expr

  let if_ (condition_info, condition) (lhs_info, lhs) (rhs_info, rhs) =
    let module S = Compiler.Static_value in
    match condition_info, lhs_info, rhs_info with
    | { S.value = S.Known_bool b; _ }, _, _ ->
      if b then
        lhs_info, lhs
      else
        rhs_info, rhs
    | { S.termination = S.Terminates_always; _ },
      { S.value = Known_int l_value; _ },
      { S.value = Known_int r_value; _ } ->
      if l_value = r_value then
        lhs_info, lhs
      else
        Compiler.Static_value.unknown, L.if_ condition lhs rhs
    | _ ->
      Compiler.Static_value.unknown, L.if_ condition lhs rhs
end
let () = let module T : Lang = Optimize(To_string) in ()

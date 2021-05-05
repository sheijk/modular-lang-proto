module type Lang =
sig
  type 'a t
  val bool : bool -> bool t
  val ( && ) : bool t -> bool t -> bool t
  val ( || ) : bool t -> bool t -> bool t
end

module To_string =
struct
  let bool b = if b then "true" else "false"
  let ( && ) lhs rhs = Printf.sprintf "(%s && %s)" lhs rhs
  let ( || ) lhs rhs = Printf.sprintf "(%s || %s)" lhs rhs
end

module Eval =
struct
  let bool b = fun _ -> b
  let ( && ) lhs rhs = fun ctx -> ((lhs ctx) && (rhs ctx))
  let ( || ) lhs rhs = fun ctx -> ((lhs ctx) || (rhs ctx))
end

let apply (f : bool -> bool -> bool) (lhs_info, lhs) (rhs_info, rhs) =
  Compiler.Info.merge lhs_info rhs_info,
  fun (ctx : Compiler.Context.t) ->
    let lhs = lhs ctx
    and rhs = rhs ctx
    in
    fun (info : Interpreter_context.t) ->
      (f (lhs info) (rhs info))

module Eval_compiled =
struct
  let bool b = Compiler.Info.make(), fun _ _ -> b

  let ( && ) = apply ( && )
  let ( || ) = apply ( || )
end

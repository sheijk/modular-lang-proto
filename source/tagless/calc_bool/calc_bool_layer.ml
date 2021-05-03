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

module Eval_compiled =
struct
  let bool b = Compiler_context.make(), fun _ -> b

  let ( && ) (lhs_info, lhs) (rhs_info, rhs) =
    Compiler_context.merge lhs_info rhs_info,
    fun ctx ->
      ((lhs ctx) && (rhs ctx))

  let ( || ) (lhs_info, lhs) (rhs_info, rhs) =
    Compiler_context.merge lhs_info rhs_info,
    fun ctx ->
      ((lhs ctx) || (rhs ctx))
end

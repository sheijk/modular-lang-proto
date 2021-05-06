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
    fun rt ->
      (f (lhs rt) (rhs rt))

module Eval_compiled =
struct
  let bool (b : bool) = Compiler.Info.make(), fun _ _ -> b

  let ( && ) lhs rhs = apply ( && ) lhs rhs
  let ( || ) lhs rhs = apply ( || ) lhs rhs
end

module Count_ast_size =
struct
  type 'a t = int

  let bool _ = 1
  let ( && ) lhs rhs = lhs + rhs + 1
  let ( || ) lhs rhs = lhs + rhs + 1
end
let () = let module T : Lang = Count_ast_size in ()

module type Lang =
sig
  type t
  val bool : bool -> t
  val ( && ) : t -> t -> t
  val ( || ) : t -> t -> t
end

module To_string =
struct
  include Empty.To_string

  let bool b = if b then "(bool true)" else "(bool false)"
  let ( && ) lhs rhs = Printf.sprintf "(%s && %s)" lhs rhs
  let ( || ) lhs rhs = Printf.sprintf "(%s || %s)" lhs rhs
end
let () = let module T : Lang = To_string in ()

module Eval(I : Interpreter.Values) =
struct
  include Empty.Eval(I)

  let bool b = fun _ -> I.bool b

  let apply f lhs rhs ctx =
    let module U = Interpreter.Value_utils(I) in
    U.match_bool_bool
      (fun lhs rhs -> I.bool @@ f lhs rhs)
      (lhs ctx)
      (rhs ctx)

  let ( && ) lhs rhs = apply ( && ) lhs rhs
  let ( || ) lhs rhs = apply ( || ) lhs rhs
end
let () = let module T : Lang = Eval(Interpreter.No_runtime) in ()

let apply (f : bool -> bool -> bool) (lhs_info, lhs) (rhs_info, rhs) =
  Compiler.Info.merge lhs_info rhs_info,
  fun (ctx : Compiler.Context.t) ->
    let lhs = lhs ctx
    and rhs = rhs ctx
    in
    fun rt ->
      let module I = Interpreter.No_runtime in
      let module U = Interpreter.Value_utils(I) in
      I.bool @@ U.match_bool_bool f (lhs rt) (rhs rt)

module Eval_compiled =
struct
  include Empty.Eval_compiled

  let bool (b : bool) = Compiler.Info.make(), fun _ _ -> I.bool b

  let ( && ) lhs rhs = apply ( && ) lhs rhs
  let ( || ) lhs rhs = apply ( || ) lhs rhs
end
let () = let module T : Lang = Eval_compiled in ()

module Count_ast_size =
struct
  type t = int

  let bool _ = 1
  let ( && ) lhs rhs = lhs + rhs + 1
  let ( || ) lhs rhs = lhs + rhs + 1
end
(* let () = let module T : Lang = Count_ast_size in () *)

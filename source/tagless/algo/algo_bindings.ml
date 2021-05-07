module type Lang =
sig
  type 'a t
  include Calc.Lang with type 'a t := 'a t
  include Algo.Lang with type 'a t := 'a t
  include Bindings.Lang with type 'a t := 'a t
end

module To_string =
struct
  include Empty.To_string
  include Calc.To_string
  include Algo.To_string
  include Bindings.To_string
end
let () = let module T : Algo.Lang = To_string in ()

module Eval(I : Interpreter.All) =
struct
  include Empty.Eval(I)
  include Calc.Eval(I)
  include Algo.Eval(I)
  include Bindings.Eval(I)
end
let () = let module T : Algo.Lang = Eval(Interpreter.Dynamic) in ()

module Eval_compiled =
struct
  include Empty.Eval_compiled
  include Calc.Eval_compiled
  include Algo.Eval_compiled
  include Bindings.Eval_compiled
end
let () = let module T : Lang = Eval_compiled in ()

module Count_ast_size =
struct
  include Empty.Count_ast_size
  include Calc.Count_ast_size
  include Algo.Count_ast_size
  include Bindings.Count_ast_size
end
let () = let module T : Lang = Count_ast_size in ()

module Optimize(L : Lang) =
struct
  include Calc.Optimize(L)

  let set name (_, value) (next_info, next) = next_info, L.set name value next
  let get name = Static_value.unknown, L.get name
  let let_ name (_, value) (expr_info, expr) = expr_info, L.let_ name value expr

  let loop (_, body) = Static_value.unknown, L.loop body
  let loop_index () = Static_value.unknown, L.loop_index ()
  let break (_, expr) = Static_value.unknown, L.break expr

  let if_ (condition_info, condition) (lhs_info, lhs) (rhs_info, rhs) =
    match condition_info, lhs_info, rhs_info with
    | { Compiler.Static_value.known_bool = Some b; _ }, _, _ ->
      if b then
        lhs_info, lhs
      else
        rhs_info, rhs
    | _,
      { Compiler.Static_value.known_int = Some l_value; _ },
      { Compiler.Static_value.known_int = Some r_value; _ } ->
      if l_value = r_value then
        lhs_info, lhs
      else
        Static_value.unknown, L.if_ condition lhs rhs
    | _ ->
      Static_value.unknown, L.if_ condition lhs rhs
end
let () = let module T : Lang = Optimize(Count_ast_size) in ()


module type Lang =
sig
  type t
end

module type Test_cases =
sig
  type t
  type expected
  type value
  type interpreter
  val tests : (expected * t) list
end

(* Used to check Tests modules in languages. See also Test_tester.Eval_cases *)
module type Test_cases_eval =
sig
  include Test_cases
  type interpreter
end

module To_st(L : Strlang.Lang) =
struct
  type t = L.t
end
let () = let module T : Lang = To_st(Strlang.Tree) in ()

module To_string =
struct
  type t = string
end
let () = let module T : Lang = To_string in ()

module Eval(I : Interpreter.Empty) =
struct
  type t = I.t -> I.value
end
let () = let module T : Lang = Eval(Interpreter.No_runtime) in ()

module Eval_compiled =
struct
  type value = Interpreter.Default_values.value
  type t = Compiler.Info.t * (Compiler.Context.t -> (unit -> value) Compiler.Result.t)
end
let () = let module T : Lang = Eval_compiled in ()

module Count_ast_size =
struct
  type t = int
end
let () = let module T : Lang = Count_ast_size in ()

module Optimize(L : Lang) =
struct
  type t = Compiler.Static_value.t * L.t
end
let () = let module T : Lang = Optimize(To_string) in ()

module Parse_rules(L : Lang) =
struct
  type t = L.t
  type reader = Strlang.Tree.t -> t
end

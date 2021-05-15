
module type Lang =
sig
  type t
end

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
  module I = Interpreter.No_runtime
  type t = Compiler.Info.t * (Compiler.Context.t -> I.t -> I.value)
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

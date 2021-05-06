
module type Lang =
sig
  type 'a t
end

module To_string =
struct
  type 'a t = string
end

module Eval_generic(I : Interpreter.Empty) =
struct
  type 'a t = I.t -> 'a
end

module Eval = Eval_generic(Interpreter.Dynamic)

module Eval_compiled =
struct
  type 'a t = Compiler.Info.t * (Compiler.Context.t -> Interpreter.No_runtime.t -> 'a)
end

module Count_ast_size =
struct
  type 'a t = int
end

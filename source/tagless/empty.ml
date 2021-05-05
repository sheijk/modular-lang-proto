
module type Lang =
sig
  type 'a t
end

module To_string =
struct
  type 'a t = string
end

module Eval =
struct
  type 'a t = Interpreter_context.t -> 'a
end

module Eval_compiled =
struct
  type 'a t = Compiler.Info.t * (Interpreter_context.t -> 'a)
end


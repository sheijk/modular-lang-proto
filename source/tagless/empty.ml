
module type Lang =
sig
  type 'a t
end

module To_string =
struct
  type 'a t = string
end
let () = let module T : Lang = To_string in ()

module Eval(I : Interpreter.Empty) =
struct
  type 'a t = I.t -> 'a
end
let () = let module T : Lang = Eval(Interpreter.No_runtime) in ()

module Eval_compiled =
struct
  type 'a t = Compiler.Info.t * (Compiler.Context.t -> Interpreter.No_runtime.t -> 'a)
end
let () = let module T : Lang = Eval_compiled in ()

module Count_ast_size =
struct
  type t = int
end
(* let () = let module T : Lang = Count_ast_size in () *)

module Optimize(L : Lang) =
struct
  type 'a t = Compiler.Static_value.t * 'a L.t
end
let () = let module T : Lang = Optimize(To_string) in ()

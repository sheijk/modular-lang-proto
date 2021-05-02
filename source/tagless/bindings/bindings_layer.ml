module type Lang =
sig
  type 'a t

  val let_ : string -> int t -> 'a t -> 'a t
  val get : string -> int t
end

module To_string =
struct
  let let_ name value expr =
    Printf.sprintf "let %s = %s in %s" name value expr

  let get name = name
end
let () = let module T : Lang = struct include Empty.To_string include To_string end in ()

module Eval =
struct
  let let_ name value expr = fun ctx ->
    let ctx' = Interpreter_context.with_variable ctx name in
    Interpreter_context.set_variable ctx' name (value ctx);
    expr ctx'

  let get name = fun ctx ->
    Interpreter_context.get_variable ctx name
end
let () = let module T : Lang = struct include Empty.Eval include Eval end in ()


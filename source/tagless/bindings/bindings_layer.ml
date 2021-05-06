module type Lang =
sig
  type 'a t

  val let_ : string -> int t -> 'a t -> 'a t
  val get : string -> int t
  val set : string -> int t -> 'a t -> 'a t
end

module To_string =
struct
  let let_ name value expr =
    Printf.sprintf "let %s = %s in %s" name value expr

  let get name = name
  let set name value expr = Printf.sprintf "%s = %s; %s" name value expr
end
let () = let module T : Lang = struct include Empty.To_string include To_string end in ()

module Eval_f(I : Interpreter.Variables) =
struct
  let let_ name value expr = fun ctx ->
    let ctx' = I.with_variable ctx name in
    I.set ctx' name (value ctx);
    expr ctx'

  let get name = fun ctx ->
    I.get ctx name

  let set name value expr = fun ctx ->
    I.set ctx name (value ctx);
    expr ctx
end
module Eval = Eval_f(Interpreter.Dynamic)
let () = let module T : Lang = struct include Empty.Eval include Eval end in ()

module Eval_compiled =
struct
  let let_ name (_v_info, value) ((e_info : Compiler.Info.t), expr) =
    e_info, fun ctx ->
      let store = ref 0 in
      let value = value ctx in
      let expr = expr (Compiler.Context.new_variable ctx name store) in
      fun rt ->
        store := value rt;
        expr rt

  let get name =
    (Compiler.Info.make ()), fun ctx ->
      let store = Compiler.Context.find_variable ctx name in
      fun _ ->
        !store

  let set name (_v_info, value) (e_info, expr) =
    e_info, fun ctx ->
      let store = Compiler.Context.find_variable ctx name in
      let value = value ctx in
      let expr = expr ctx in
      fun rt ->
        store := value rt;
        expr rt
end

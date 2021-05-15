module type Lang =
sig
  type 'a t

  val let_ : string -> int t -> 'a t -> 'a t
  val get : string -> int t
  val set : string -> int t -> 'a t -> 'a t
end

module To_string =
struct
  include Empty.To_string
  let let_ name value expr =
    Printf.sprintf "let %s = %s in %s" name value expr

  let get name = name
  let set name value expr = Printf.sprintf "%s = %s; %s" name value expr
end
let () = let module T : Lang = struct include Empty.To_string include To_string end in ()

module Eval(I : Interpreter.Variables) =
struct
  include Empty.Eval(I)

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
let () = let module T : Lang = Eval(Interpreter.Dynamic) in ()

module Eval_compiled =
struct
  include Empty.Eval_compiled

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
let () = let module T : Lang = Eval_compiled in ()

module Count_ast_size =
struct
  type t = int

  let let_ _name value expr = value + expr + 1
  let get _name = 1
  let set _name value expr = value + expr + 1
end
(* let () = let module T : Lang = Count_ast_size in () *)

module Optimize(L : Lang) =
struct
  include Empty.Optimize(L)

  let set name (_, value) (next_info, next) = next_info, L.set name value next
  let get name = Compiler.Static_value.unknown, L.get name
  let let_ name (_, value) (expr_info, expr) = expr_info, L.let_ name value expr
end
let () = let module T : Lang = Optimize(To_string) in ()

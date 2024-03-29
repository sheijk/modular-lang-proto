module type Lang =
sig
  type t

  val let_ : string -> t -> t -> t
  val get : string -> t
  val set : string -> t -> t -> t
end

module To_st(S : Strlang.Lang) =
struct
  include Empty.To_st(S)

  let let_ name value expr =
    S.tree [S.leaf "let"; S.leaf name; value; expr]

  let get name =
    S.leaf name

  let set name value expr =
    S.tree [S.leaf "set"; S.leaf name; value; expr]
end
module To_string = To_st(Strlang.To_string)
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
  module I = Interpreter.No_runtime

  let let_ name (_v_info, value) ((e_info : Compiler.Info.t), expr) =
    e_info, fun ctx ->
      let open Compiler.Result.Syntax in
      let store = ref (I.int 0) in
      let+ value = value ctx
      and+ expr = expr (Compiler.Context.new_variable ctx name store)
      in
      fun rt ->
        store := value rt;
        expr rt

  let get name =
    (Compiler.Info.make ()), fun ctx ->
      let open Compiler.Result.Syntax in
      let+ store = Compiler.Context.find_variable ctx name in
      fun _ -> !store

  let set name (_v_info, value) (e_info, expr) =
    e_info, fun ctx ->
      let open Compiler.Result.Syntax in
      let+ store = Compiler.Context.find_variable ctx name
      and+ value = value ctx
      and+ expr = expr ctx
      in
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

module Parse_rules(L : Lang) : (Parser.Rules with type t = L.t) =
struct
  include Empty.Parse_rules(L)

  let readers =
    Strlang.Tree.[
      "let", (fun parse_rec parse st ->
          match st with
          | Tree [_; Leaf name; value; expr] ->
            let rec parse_nested = function
              | Leaf n when n = name -> L.get name
              | st -> parse_rec parse_nested st
            in
            L.let_ name (parse value) (parse_nested expr)
          | st -> Parser.parse_error_at "invalid let" st);
      "get", (fun _parse_rec _parse st ->
          match st with
          | Tree [_; Leaf name] ->
            L.get name
          | st -> Parser.parse_error_at "invalid get" st);
      "set", (fun _parse_rec parse st ->
          match st with
          | Tree [_; Leaf name; value; expr] ->
            L.set name (parse value) (parse expr)
          | st -> Parser.parse_error_at "invalid break" st);
    ]
end

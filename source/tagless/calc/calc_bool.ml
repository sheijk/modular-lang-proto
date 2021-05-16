module type Lang =
sig
  type t
  val bool : bool -> t
  val ( && ) : t -> t -> t
  val ( || ) : t -> t -> t
end

module To_st(S : Strlang.Lang) =
struct
  include Empty.To_st(S)

  let bool b =
    S.tree [
      S.leaf "bool";
      S.leaf @@ string_of_bool b]

  let ( && ) lhs rhs =
    S.tree [ S.leaf "&&"; lhs; rhs ]

  let ( || ) lhs rhs =
    S.tree [ S.leaf "||"; lhs; rhs ]
end
module To_string = To_st(Strlang.To_string)
let () = let module T : Lang = To_string in ()

module Eval(I : Interpreter.Values) =
struct
  include Empty.Eval(I)

  let bool b = fun _ -> I.bool b

  let apply f lhs rhs ctx =
    let module U = Interpreter.Value_utils(I) in
    U.match_bool_bool
      (fun lhs rhs -> I.bool @@ f lhs rhs)
      (lhs ctx)
      (rhs ctx)

  let ( && ) lhs rhs = apply ( && ) lhs rhs
  let ( || ) lhs rhs = apply ( || ) lhs rhs
end
let () = let module T : Lang = Eval(Interpreter.No_runtime) in ()

let apply (f : bool -> bool -> bool) (lhs_info, lhs) (rhs_info, rhs) =
  Compiler.Info.merge lhs_info rhs_info,
  fun (ctx : Compiler.Context.t) ->
    let lhs = lhs ctx
    and rhs = rhs ctx
    in
    fun rt ->
      let module I = Interpreter.No_runtime in
      let module U = Interpreter.Value_utils(I) in
      I.bool @@ U.match_bool_bool f (lhs rt) (rhs rt)

module Eval_compiled =
struct
  include Empty.Eval_compiled

  let bool (b : bool) = Compiler.Info.make(), fun _ _ -> I.bool b

  let ( && ) lhs rhs = apply ( && ) lhs rhs
  let ( || ) lhs rhs = apply ( || ) lhs rhs
end
let () = let module T : Lang = Eval_compiled in ()

module Count_ast_size =
struct
  type t = int

  let bool _ = 1
  let ( && ) lhs rhs = lhs + rhs + 1
  let ( || ) lhs rhs = lhs + rhs + 1
end
let () = let module T : Lang = Count_ast_size in ()

module Parse_rules(L : Lang) : (Parser.Rules with type t = L.t) =
struct
  include Empty.Parse_rules(L)

  let readers =
    Strlang.Tree.[
      "bool", (fun _parse st ->
          match st with
          | Tree [_; Leaf "true"] -> L.bool true
          | Tree [_; Leaf "false"] -> L.bool false
          | st -> Parser.parse_error_at "invalid bool literal" st);
      "&&", Parser.binop L.( && );
      "||", Parser.binop L.( || );
    ]
end


module type Lang =
sig
  type t
  val int : int -> t
  val ( +. ) : t -> t -> t
  val ( -. ) : t -> t -> t
  val ( *. ) : t -> t -> t
  val ( /. ) : t -> t -> t
end

module Tests(L : Lang) =
struct
  type t = L.t
  module I = Interpreter.Dynamic
  type value = I.value
  type expected = value option
  type interpreter = I.t

  let is_int n = Some (I.int n)

  let tests = L.[
      is_int 3, int 1 +. int 2;
      is_int 1, int 1;
      is_int 99, int 99;
      is_int 106, int 100 +. int 3 *. int 2;
    ]
end

module To_st(S : Strlang.Lang) =
struct
  include Empty.To_st(S)

  let int i = S.tree [S.leaf "int"; S.leaf (string_of_int i)]
  let ( +. ) lhs rhs = S.tree [S.leaf "+."; lhs; rhs]
  let ( -. ) lhs rhs = S.tree [S.leaf "-."; lhs; rhs]
  let ( *. ) lhs rhs = S.tree [S.leaf "*."; lhs; rhs]
  let ( /. ) lhs rhs = S.tree [S.leaf "/."; lhs; rhs]
end
module To_string = To_st(Strlang.To_string)
let () = let module T : Lang = To_string in ()
let () = let module T : Empty.Test_cases_eval = Tests(To_string) in ()

module Eval(I : Interpreter.Values) =
struct
  include Empty.Eval(I)

  let int n = fun _ -> I.int n

  let apply f lhs rhs = fun ctx ->
    let module U = Interpreter.Value_utils(I) in
    I.int @@ U.match_int_int f (lhs ctx) (rhs ctx)

  let ( +. ) lhs rhs = apply ( + ) lhs rhs
  let ( -. ) lhs rhs = apply ( - ) lhs rhs
  let ( *. ) lhs rhs = apply ( * ) lhs rhs
  let ( /. ) lhs rhs = apply ( / ) lhs rhs
end
let () = let module T : Lang = Eval(Interpreter.No_runtime) in ()

let apply f (lhs_info, lhs) (rhs_info, rhs) =
  Compiler.Info.merge lhs_info rhs_info,
  fun ctx ->
    let open Compiler.Result.Syntax in
    let+ lhs = lhs ctx
    and+ rhs = rhs ctx
    in
    fun rt ->
      let module I = Interpreter.No_runtime in
      let module U = Interpreter.Value_utils(I) in
      I.int @@ U.match_int_int f (lhs rt) (rhs rt)

module Eval_compiled =
struct
  include Empty.Eval_compiled

  let int i = Compiler.Info.make(), fun _ctx ->
      Compiler.Result.ok @@
      fun _rt -> Interpreter.Default_values.int i

  let ( +. ) = apply ( + )
  let ( -. ) = apply ( - )
  let ( *. ) = apply ( * )
  let ( /. ) = apply ( / )
end
let () = let module T : Lang = Eval_compiled in ()

let add_plus_one lhs rhs = lhs + rhs + 1

module Count_ast_size =
struct
  type t = int

  let int _ = 1

  let ( +. ) = add_plus_one
  let ( -. ) = add_plus_one
  let ( *. ) = add_plus_one
  let ( /. ) = add_plus_one
end
let () = let module T : Lang = Count_ast_size in ()

module Parse_rules(L : Lang) : (Parser.Rules with type t = L.t) =
struct
  include Empty.Parse_rules(L)

  let readers =
    Strlang.Tree.[
      "int", (fun _parse_rec _parse st ->
          match st with
          | Tree [_; Leaf value] -> L.int (int_of_string value)
          | st -> Parser.parse_error_at "invalid int literal" st);
      "+.", Parser.binop L.( +. );
      "-.", Parser.binop L.( -. );
      "*.", Parser.binop L.( *. );
      "/.", Parser.binop L.( /. );
    ]
end

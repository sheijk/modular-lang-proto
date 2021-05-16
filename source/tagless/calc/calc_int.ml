module type Lang =
sig
  type t
  val int : int -> t
  val ( +. ) : t -> t -> t
  val ( -. ) : t -> t -> t
  val ( *. ) : t -> t -> t
  val ( /. ) : t -> t -> t
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
    let lhs = lhs ctx
    and rhs = rhs ctx
    in
    fun rt ->
      let module I = Interpreter.No_runtime in
      let module U = Interpreter.Value_utils(I) in
      I.int @@ U.match_int_int f (lhs rt) (rhs rt)

module Eval_compiled =
struct
  include Empty.Eval_compiled

  let int i = Compiler.Info.make(), fun _ _ -> I.int i

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
(* let () = let module T : Lang = Count_ast_size in () *)

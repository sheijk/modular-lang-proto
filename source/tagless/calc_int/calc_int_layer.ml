module type Lang =
sig
  type 'a t
  val int : int -> int t
  val ( +. ) : int t -> int t -> int t
  val ( -. ) : int t -> int t -> int t
  val ( *. ) : int t -> int t -> int t
  val ( /. ) : int t -> int t -> int t
end

module To_string =
struct
  include Empty.To_string

  let int = string_of_int
  let ( +. ) lhs rhs = Printf.sprintf "(%s +. %s)" lhs rhs
  let ( -. ) lhs rhs = Printf.sprintf "(%s -. %s)" lhs rhs
  let ( *. ) lhs rhs = Printf.sprintf "(%s *. %s)" lhs rhs
  let ( /. ) lhs rhs = Printf.sprintf "(%s /. %s)" lhs rhs
end
let () = let module T : Lang = To_string in ()

module Eval(I : Interpreter.Empty) =
struct
  include Empty.Eval(I)

  let int n = fun _ -> n
  let ( +. ) lhs rhs = fun ctx -> (lhs ctx) + (rhs ctx)
  let ( -. ) lhs rhs = fun ctx -> (lhs ctx) - (rhs ctx)
  let ( *. ) lhs rhs = fun ctx -> (lhs ctx) * (rhs ctx)
  let ( /. ) lhs rhs = fun ctx -> (lhs ctx) / (rhs ctx)
end
let () = let module T : Lang = Eval(Interpreter.No_runtime) in ()

let apply f (lhs_info, lhs) (rhs_info, rhs) =
  Compiler.Info.merge lhs_info rhs_info,
  fun ctx ->
    let lhs = lhs ctx
    and rhs = rhs ctx
    in
    fun ctx ->
      (f (lhs ctx) (rhs ctx))

module Eval_compiled =
struct
  include Empty.Eval_compiled

  let int i = Compiler.Info.make(), fun _ _ -> i

  let ( +. ) = apply ( + )
  let ( -. ) = apply ( - )
  let ( *. ) = apply ( * )
  let ( /. ) = apply ( / )
end
let () = let module T : Lang = Eval_compiled in ()

let add_plus_one lhs rhs = lhs + rhs + 1

module Count_ast_size =
struct
  type 'a t = int

  let int _ = 1

  let ( +. ) = add_plus_one
  let ( -. ) = add_plus_one
  let ( *. ) = add_plus_one
  let ( /. ) = add_plus_one
end
let () = let module T : Lang = Count_ast_size in ()

module type Lang =
sig
  type 'a t
  include Calc_int.Lang with type 'a t := 'a t
  include Calc_bool.Lang with type 'a t := 'a t

  val ( =. ) : int t -> int t -> bool t
  val ( <. ) : int t -> int t -> bool t
  val ( >. ) : int t -> int t -> bool t
end

module To_string =
struct
  include Calc_int.To_string
  include Calc_bool.To_string

  let ( =. ) = Printf.sprintf "(%s =. %s)"
  let ( <. ) = Printf.sprintf "(%s <. %s)"
  let ( >. ) = Printf.sprintf "(%s >. %s)"
end
let () = let module T : Lang = To_string in ()

module Eval(I : Interpreter.Empty) =
struct
  include Empty.Eval(I)
  include Calc_bool.Eval(I)
  include Calc_int.Eval(I)

  let ( =. ) lhs rhs = fun ctx -> (lhs ctx) = (rhs ctx)
  let ( <. ) lhs rhs = fun ctx -> (lhs ctx) < (rhs ctx)
  let ( >. ) lhs rhs = fun ctx -> (lhs ctx) > (rhs ctx)
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
  include Calc_bool.Eval_compiled
  include Calc_int.Eval_compiled

  let ( =. ) = apply ( = )
  let ( <. ) = apply ( < )
  let ( >. ) = apply ( > )
end
let () = let module T : Lang = Eval_compiled in ()

let add_plus_one lhs rhs = lhs + rhs + 1

module Count_ast_size =
struct
  include Empty.Count_ast_size
  include Calc_bool.Count_ast_size
  include Calc_int.Count_ast_size

  let ( =. ) = add_plus_one
  let ( <. ) = add_plus_one
  let ( >. ) = add_plus_one
end
let () = let module T : Lang = Count_ast_size in ()


module Static_value =
struct
  type t = {
    known_int : int option;
    known_bool : bool option;
  }

  let to_string = function
    | { known_int = None; known_bool = None } -> "dynamic"
    | { known_int = Some i; known_bool = None } -> string_of_int i
    | { known_int = None; known_bool = Some b } -> string_of_bool b
    | { known_int = Some _; known_bool = Some _ } -> "invalid, internal error"

  let is_known = function
    | { known_bool = Some _; _ }
    | { known_int = Some _; _ } ->
      true
    | _ ->
      false

  let unknown = { known_int = None; known_bool = None; }

  let bool b = { unknown with known_bool = Some b; }
  let int i = { unknown with known_int = Some i; }
end

module Optimize(L : Lang) =
struct
  type 'a t = Static_value.t * 'a L.t

  let with_l f_info f_l = fun (lhs_info, lhs) (rhs_info, rhs) ->
    f_info lhs_info rhs_info, f_l lhs rhs

  let bool b = Static_value.bool b, L.bool b
  let int i = Static_value.int i, L.int i

  let combine_int l_value s_value pred l_op (lhs_info, lhs) (rhs_info, rhs) =
    let module S = Static_value in
    match lhs_info, rhs_info with
    | { S.known_int = Some a_val; _ }, { S.known_int = Some b_val; _ } ->
      let result = pred a_val b_val in
      s_value result, l_value result
    | _, _ ->
      S.unknown, l_op lhs rhs

  let combine_int_pred = combine_int L.bool Static_value.bool
  let combine_int_f = combine_int L.int Static_value.int

  let combine_bool_pred pred l_op (lhs_info, lhs) (rhs_info, rhs) =
    let module S = Static_value in
    match lhs_info, rhs_info with
    | { S.known_bool = Some a_val; _ }, { S.known_bool = Some b_val; _ } ->
      let result = pred a_val b_val in
      Static_value.bool result, L.bool result
    | _, _ ->
      S.unknown, l_op lhs rhs

  let ( >. ) = combine_int_pred ( > ) L.( >. )
  let ( <. ) = combine_int_pred ( < ) L.( <. )
  let ( =. ) = combine_int_pred ( = ) L.( =. )

  let ( && ) = combine_bool_pred ( && ) L.( && )
  let ( || ) = combine_bool_pred ( || ) L.( || )

  let ( +. ) = combine_int_f ( + ) L.( +. )
  let ( -. ) = combine_int_f ( - ) L.( -. )
  let ( *. ) = combine_int_f ( * ) L.( *. )
  let ( /. ) = combine_int_f ( / ) L.( /. )
end
let () = let module T : Lang = Optimize(Count_ast_size) in ()

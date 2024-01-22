module type Lang =
sig
  type t
  include Calc_int.Lang with type t := t
  include Calc_bool.Lang with type t := t

  val ( =. ) : t -> t -> t
  val ( <. ) : t -> t -> t
  val ( >. ) : t -> t -> t
end

module Tests(L : Lang) =
struct
  type t = L.t
  module I = Interpreter.No_runtime
  type value = Interpreter.Default_values.value

  let is_int n = Some (I.int n)
  let is_bool b = Some (I.bool b)

  let tests =
    let module Ci = Calc_int.Tests(L) in
    Ci.tests @ L.[
        is_int 1, int 1;
        is_int 15, int 10 +. int 5;
        is_int 123, (int 1 *. int 10 +. int 2) *. int 10 +. int 3;
        (* Some 5, int 10 / int 2; *)

        is_bool true, bool true;
        is_bool true, int 4 <. int 10;
        is_bool false, int 4 >. int 10;
        is_bool true, int 3 =. int 3;
        is_bool false, int 3 =. int 4;
        is_bool true, int 3 >. int (-10);
        is_bool false, int 3 >. int 3;

        is_bool true, bool true || bool false;
        is_bool false, bool false || bool false;
        is_bool false, bool true && bool false;
      ]

  (* Float tests from ast/Calc *)
  (*   run (i 8) Build.(to_i (f 8.)); *)
  (*   run (i 74) Build.(to_i (f 64.) + i 10); *)
  (*   run (f 8.) Build.(f 13. - f 5.); *)
  (*   run (f 10.) Build.(to_f @@ i 8 + i 2); *)
  (*   run (f 10.) Build.(f 8. + to_f (i 2)); *)
  (*   run (f 24.) Build.(to_f (i 16) + f 8.); *)
  (*   run (f 8.) Build.(f 3. + i 5); *)
end

module To_st(S : Strlang.Lang) =
struct
  include Calc_int.To_st(S)
  include Calc_bool.To_st(S)

  let ( =. ) lhs rhs = S.tree [S.leaf "=."; lhs; rhs]
  let ( <. ) lhs rhs = S.tree [S.leaf "<."; lhs; rhs]
  let ( >. ) lhs rhs = S.tree [S.leaf ">."; lhs; rhs]
end
module To_string = To_st(Strlang.To_string)
let () = let module T : Lang = To_string in ()

module Eval(I : Interpreter.Values) =
struct
  include Empty.Eval(I)
  include Calc_bool.Eval(I)
  include Calc_int.Eval(I)

  let apply f lhs rhs = fun ctx ->
    let module U = Interpreter.Value_utils(I) in
    I.bool @@ U.match_int_int f (lhs ctx) (rhs ctx)

  let ( =. ) lhs rhs = apply ( = ) lhs rhs
  let ( <. ) lhs rhs = apply ( < ) lhs rhs
  let ( >. ) lhs rhs = apply ( > ) lhs rhs
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
      I.bool @@ U.match_int_int f (lhs rt) (rhs rt)

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
(* let () = let module T : Lang = Count_ast_size in () *)

module Optimize(L : Lang) =
struct
  module Static_value = Compiler.Static_value
  type t = Static_value.t * L.t

  let with_l f_info f_l = fun (lhs_info, lhs) (rhs_info, rhs) ->
    f_info lhs_info rhs_info, f_l lhs rhs

  let bool b = Static_value.bool b, L.bool b
  let int i = Static_value.int i, L.int i

  let combine_int (l_value : 'a -> L.t) s_value pred l_op (lhs_info, lhs) (rhs_info, rhs) =
    let module S = Static_value in
    match lhs_info, rhs_info with
    | { S.value = S.Known_int a_val; _ }, { S.value = S.Known_int b_val; _ } ->
      let result = pred a_val b_val in
      let info = Static_value.merge lhs_info rhs_info in
      let static_value = s_value info result in
      let lang_value = l_value result in
      static_value, lang_value
    | _, _ ->
      let static_value = S.merge lhs_info rhs_info in
      let lang_value = l_op lhs rhs in
      static_value, lang_value

  let combine_int_pred = combine_int L.bool Static_value.with_bool
  let combine_int_f = combine_int L.int Static_value.with_int

  let combine_bool_pred pred l_op (lhs_info, lhs) (rhs_info, rhs) =
    let module S = Static_value in
    match lhs_info, rhs_info with
    | { S.value = S.Known_bool a_val; _ }, { S.value = S.Known_bool b_val; _ } ->
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
let () = let module T : Lang = Optimize(To_string) in ()

module Parse_rules(L : Lang) : (Parser.Rules with type t = L.t) =
struct
  include Empty.Parse_rules(L)

  let readers =
    let module B = Calc_bool.Parse_rules(L) in
    let module I = Calc_int.Parse_rules(L) in
    B.readers @
    I.readers @
    [
      "=.", Parser.binop L.( =. );
      "<.", Parser.binop L.( <. );
      ">.", Parser.binop L.( >. );
    ]
end

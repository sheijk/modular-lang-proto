
module Type =
struct
  type t = Float | Int | Bool | Error of string [@@deriving show]
end

type comparison_t = Less | Greater | Equal [@@deriving show]

let comparison_function = function
  | Less -> ( < )
  | Greater -> ( > )
  | Equal -> ( = )

module type Lang =
sig
  type 'a t
  include Calc_int.Lang with type 'a t := 'a t
  include Calc_bool.Lang with type 'a t := 'a t

  val int_to_float : int t -> float t
  val float_to_int : float t -> int t
  val int_equal : int t -> int t -> bool t
  val int_less : int t -> int t -> bool t
  val int_greater : int t -> int t -> bool t
end

module To_string =
struct
  type 'a t = string
  include Calc_int.To_string'(struct type 'a t = string end)
  include Calc_bool.To_string'(struct type 'a t = string end)

  let int_to_float = Printf.sprintf "(int_to_float %s)"
  let float_to_int = Printf.sprintf "(float_to_int %s)"
  let int_equal = Printf.sprintf "(int_equal %s %s)"
  let int_less = Printf.sprintf "(int_less %s %s)"
  let int_greater = Printf.sprintf "(int_greater %s %s)"
end
let () = let module T : Lang = To_string in ()

type value =
  | Int_value of int
  | Float_value of float
  | Bool_value of bool
[@@deriving show]

let equal_value l r =
  match (l, r) with
  | (Bool_value l), (Bool_value r) ->
    l = r
  | (Int_value l), (Int_value r) ->
    l = r
  | (Float_value l), (Float_value r) ->
    l = r
  | (Float_value _), (Int_value _ | Bool_value _)
  | (Int_value _), (Float_value _ | Bool_value _)
  | (Bool_value _), (Float_value _ | Int_value _) ->
    false

module Eval =
struct
  type 'a t = unit -> 'a

  include Calc_bool.Eval'(struct type 'a t = unit -> 'a end)
  include Calc_int.Eval'(struct type 'a t = unit -> 'a end)

  let error message = failwith message

  let int_to_float value =
    fun () -> float_of_int (value())

  let float_to_int value =
    fun () -> int_of_float (value())

  let int_equal lhs rhs = fun () -> (lhs()) = (rhs())
  let int_less lhs rhs = fun () -> (lhs()) < (rhs())
  let int_greater lhs rhs = fun () -> (lhs()) > (rhs())
end
let () = let module T : Lang = Eval in ()

(* module Build =
 * struct
 *   let to_bool = function
 *     | Bool_expr ie -> ie
 *     | expr -> Calc_bool.Other expr
 * 
 *   let to_int = function
 *     | Int_expr ie -> ie
 *     | expr -> Calc_int.Other expr
 * 
 *   let to_float = function
 *     | Float_expr fe -> fe
 *     | expr -> Calc_float.Other expr
 * 
 *   let from_bool bexpr = Bool_expr bexpr
 *   let from_int iexpr = Int_expr iexpr
 *   let from_float fexpr = Float_expr fexpr
 * 
 *   let b b = from_bool (Calc_bool.Literal b)
 *   let i i = from_int (Calc_int.Literal i)
 *   let f f = from_float (Calc_float.Literal f)
 * 
 *   let make_num_op op_int op_float = fun l r ->
 *     match (type_of l, type_of r) with
 *     | Type.Int, Type.Int ->
 *       from_int (Calc_int.BinOp (to_int l, op_int, to_int r))
 *     | Type.Float, Type.Float ->
 *       from_float (Calc_float.BinOp (to_float l, op_float, to_float r))
 *     | _ ->
 *       Error "type error"
 * 
 *   let ( + ) = make_num_op Calc_int.Add Calc_float.Add
 *   let ( - ) = make_num_op Calc_int.Sub Calc_float.Sub
 *   let ( * ) = make_num_op Calc_int.Mul Calc_float.Mul
 *   let ( / ) = make_num_op Calc_int.Div Calc_float.Div
 * 
 *   let ( && ) l r = from_bool (Calc_bool.BinOp (to_bool l, Calc_bool.And, to_bool r))
 *   let ( || ) l r = from_bool (Calc_bool.BinOp (to_bool l, Calc_bool.Or, to_bool r))
 * 
 *   let ( < ) l r = Comparison (l, Less, r)
 *   let ( > ) l r = Comparison (l, Greater, r)
 *   let ( = ) l r = Comparison (l, Equal, r)
 * 
 *   let to_f e = To_float e
 *   let to_i e = To_int e
 * end
 * 
 * let show_void _ () = ()
 * let eval_void () = failwith "Cannot eval ()" *)

(* let run expect expr =
 *   Printf.printf "Running\n  %s\n  => %s\n"
 *     (show show_void expr)
 *     (match (expect, eval eval_void expr) with
 *      | Int_value expect, (Int_value got as gotv) ->
 *        let value_str = show_value gotv in
 *        if expect = got then
 *          value_str
 *        else
 *          Printf.sprintf "*error: expected %d but got %s" expect value_str
 *      | Float_value expect, (Float_value got as gotv) ->
 *        let value_str = show_value gotv in
 *        if expect -. got < epsilon_float then
 *          value_str
 *        else
 *          Printf.sprintf "*error: expected %f but got %s" expect value_str
 *      | Bool_value expect, (Bool_value got as gotv) ->
 *        let value_str = show_value gotv in
 *        if expect = got then
 *          value_str
 *        else
 *          Printf.sprintf "*error: expected %b but got %s" expect value_str
 *      | Float_value _, (Int_value _ | Bool_value _)
 *      | Int_value _, (Float_value _ | Bool_value _)
 *      | Bool_value _, (Int_value _ | Float_value _) ->
 *        "*error: can't mix float and int expressions"
 *      | exception Failure str ->
 *        Printf.sprintf "*exception %s*" str) *)

(* let demo() =
 *   let i i = Int_value i in
 *   let f f = Float_value f in
 *   let b b = Bool_value b in
 * 
 *   (\* run @@ Build.(i 10 + to_f (i 2 * i 3)); *\)
 *   run (i 15) Build.(i 10 + i 5);
 *   run (f 8.) Build.(f 13. - f 5.);
 *   run (i 123) Build.((i 1 * i 10 + i 2) * i 10 + i 3);
 *   run (i 5) Build.(i 10 / i 2);
 * 
 *   run (f 10.) Build.(to_f @@ i 8 + i 2);
 *   run (f 10.) Build.(f 8. + to_f (i 2));
 *   run (f 24.) Build.(to_f (i 16) + f 8.);
 *   run (i 8) Build.(to_i (f 8.));
 *   run (i 74) Build.(to_i (f 64.) + i 10);
 * 
 *   run (b true) Build.(i 4 < i 10);
 *   run (b false) Build.(i 4 > i 10);
 *   run (b true) Build.(i 3 = i 3);
 *   run (b false) Build.(i 3 = i 4);
 *   run (b true) Build.(i 3 > i (-10));
 *   run (b false) Build.(i 3 > i 3);
 * 
 *   run (b true) Build.(b true || b false);
 *   run (b false) Build.(b true && b false);
 * 
 *   print_endline "ERROR CASES:";
 *   run (f 8.) Build.(f 3. + i 5);
 *   () *)


module Type =
struct
  type t = Float | Int | Bool | Error of string [@@deriving show]
end

module Expr =
struct
  type comparison_t = Less | Greater | Equal [@@deriving show]

  let comparison_function = function
    | Less -> ( < )
    | Greater -> ( > )
    | Equal -> ( = )

  type 'a t =
    | Int_expr of 'a t Calc_int.Expr.t
    | Float_expr of 'a t Calc_float.Expr.t
    | Bool_expr of 'a t Calc_bool.Expr.t
    | To_float of 'a t
    | To_int of 'a t
    | Comparison of 'a t * comparison_t * 'a t
    | Other of 'a
    | Error of string
  [@@deriving show]

  type value =
    | Int_value of int
    | Float_value of float
    | Bool_value of bool
  [@@deriving show]

  let type_of = function
    | Int_expr _
    | To_int _ ->
      Type.Int
    | Float_expr _
    | To_float _ ->
      Type.Float
    | Bool_expr _
    | Comparison _ ->
      Type.Bool
    | Other _ ->
      Type.Error "invalid embedded type"
    | Error msg ->
      Type.Error msg

  let rec eval eval_other = function
    | Int_expr expr ->
      Int_value (Calc_int.Expr.eval (eval_int eval_other) expr)
    | Float_expr expr ->
      Float_value (Calc_float.Expr.eval (eval_float eval_other) expr)
    | Bool_expr expr ->
      Bool_value (Calc_bool.Expr.eval (eval_bool eval_other) expr)
    | To_float iexpr ->
      begin match eval eval_other iexpr with
        | Int_value i ->
          Float_value (Float.of_int i)
        | Float_value _ as fv ->
          fv
        | Bool_value b ->
          Float_value (if b then 1.0 else 0.0)
      end
    | To_int fexpr ->
      begin match eval eval_other fexpr with
        | Float_value f ->
          Int_value (Float.to_int f)
        | Int_value _ as iv ->
          iv
        | Bool_value b ->
          Int_value (if b then 1 else 0)
      end
    | Comparison (lhs, op, rhs) ->
      begin match (eval eval_other lhs, eval eval_other rhs) with
        | Int_value lhs, Int_value rhs ->
          Bool_value ((comparison_function op) lhs rhs)
        | Float_value lhs, Float_value rhs ->
          Bool_value ((comparison_function op) lhs rhs)
        | _ ->
          failwith "Comparison needs two numeric parameters of the same type"
      end
    | Other expr ->
      eval_other expr
    | Error str ->
      failwith str

  and eval_float eval_other expr =
    match eval eval_other expr with
    | Float_value f -> f
    | _ -> failwith "expected float"

  and eval_int eval_other expr =
    match eval eval_other expr with
    | Int_value i -> i
    | _ -> failwith "expected int"

  and eval_bool eval_other expr =
    match eval eval_other expr with
    | Bool_value b -> b
    | _ -> failwith "expected bool"
end

module Build =
struct
  let i i = Expr.Int_expr (Calc_int.Expr.Literal i)
  let f f = Expr.Float_expr (Calc_float.Expr.Literal f)
  let b b = Expr.Bool_expr (Calc_bool.Expr.Literal b)

  let as_float = function
    | Expr.Float_expr fe -> fe
    | expr -> Calc_float.Expr.Other expr

  let as_int = function
    | Expr.Int_expr ie -> ie
    | expr -> Calc_int.Expr.Other expr

  let as_bool = function
    | Expr.Bool_expr ie -> ie
    | expr -> Calc_bool.Expr.Other expr

  let make_op op_int op_float = fun l r ->
    match (Expr.type_of l, Expr.type_of r) with
    | Type.Int, Type.Int ->
      Expr.Int_expr (Calc_int.Expr.BinOp (as_int l, op_int, as_int r))
    | Type.Float, Type.Float ->
      Expr.Float_expr (Calc_float.Expr.BinOp (as_float l, op_float, as_float r))
    | _ ->
      Expr.Error "type error"

  let ( + ) = make_op Calc_int.Expr.Add Calc_float.Expr.Add
  let ( - ) = make_op Calc_int.Expr.Sub Calc_float.Expr.Sub
  let ( * ) = make_op Calc_int.Expr.Mul Calc_float.Expr.Mul
  let ( / ) = make_op Calc_int.Expr.Div Calc_float.Expr.Div

  let ( && ) l r = Expr.Bool_expr (Calc_bool.Expr.BinOp (as_bool l, Calc_bool.Expr.And, as_bool r))
  let ( || ) l r = Expr.Bool_expr (Calc_bool.Expr.BinOp (as_bool l, Calc_bool.Expr.Or, as_bool r))

  let ( < ) l r = Expr.Comparison (l, Expr.Less, r)
  let ( > ) l r = Expr.Comparison (l, Expr.Greater, r)
  let ( = ) l r = Expr.Comparison (l, Expr.Equal, r)

  let to_f e = Expr.To_float e
  let to_i e = Expr.To_int e
end

let show_void _ () = ()
let eval_void () = failwith "Cannot eval ()"

let run expect expr =
  Printf.printf "Running\n  %s\n  => %s\n"
    (Expr.show show_void expr)
    (match (expect, Expr.eval eval_void expr) with
     | Expr.Int_value expect, (Expr.Int_value got as gotv) ->
       let value_str = Expr.show_value gotv in
       if expect = got then
         value_str
       else
         Printf.sprintf "*error: expected %d but got %s" expect value_str
     | Expr.Float_value expect, (Expr.Float_value got as gotv) ->
       let value_str = Expr.show_value gotv in
       if expect -. got < epsilon_float then
         value_str
       else
         Printf.sprintf "*error: expected %f but got %s" expect value_str
     | Expr.Bool_value expect, (Expr.Bool_value got as gotv) ->
       let value_str = Expr.show_value gotv in
       if expect = got then
         value_str
       else
         Printf.sprintf "*error: expected %b but got %s" expect value_str
     | Expr.Float_value _, (Expr.Int_value _ | Expr.Bool_value _)
     | Expr.Int_value _, (Expr.Float_value _ | Expr.Bool_value _)
     | Expr.Bool_value _, (Expr.Int_value _ | Expr.Float_value _) ->
       "*error: can't mix float and int expressions"
     | exception Failure str ->
       Printf.sprintf "*exception %s*" str)

let demo() =
  let i i = Expr.Int_value i in
  let f f = Expr.Float_value f in
  let b b = Expr.Bool_value b in

  (* run @@ Build.(i 10 + to_f (i 2 * i 3)); *)
  run (i 15) Build.(i 10 + i 5);
  run (f 8.) Build.(f 13. - f 5.);
  run (i 123) Build.((i 1 * i 10 + i 2) * i 10 + i 3);
  run (i 5) Build.(i 10 / i 2);

  run (f 10.) Build.(to_f @@ i 8 + i 2);
  run (f 10.) Build.(f 8. + to_f (i 2));
  run (f 24.) Build.(to_f (i 16) + f 8.);
  run (i 8) Build.(to_i (f 8.));
  run (i 74) Build.(to_i (f 64.) + i 10);

  run (b true) Build.(i 4 < i 10);
  run (b false) Build.(i 4 > i 10);
  run (b true) Build.(i 3 = i 3);
  run (b false) Build.(i 3 = i 4);
  run (b true) Build.(i 3 > i (-10));
  run (b false) Build.(i 3 > i 3);

  run (b true) Build.(b true || b false);
  run (b false) Build.(b true && b false);

  print_endline "ERROR CASES:";
  run (f 8.) Build.(f 3. + i 5);
  ()

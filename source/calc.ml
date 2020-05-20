
module Type =
struct
  type t = Float | Int | Error of string [@@deriving show]
end

module Expr =
struct
  type 'a t =
    | Int_expr of 'a t Calc_int.Expr.t
    | Float_expr of 'a t Calc_float.Expr.t
    | To_float of 'a t
    | To_int of 'a t
    | Other of 'a
    | Error of string
  [@@deriving show]

  type value =
    | Int_value of int
    | Float_value of float
  [@@deriving show]

  let type_of = function
    | Int_expr _
    | To_int _ ->
      Type.Int
    | Float_expr _
    | To_float _ ->
      Type.Float
    | Other _ ->
      Type.Error "invalid embedded type"
    | Error msg ->
      Type.Error msg

  let rec eval eval_other = function
    | Int_expr expr -> Int_value (Calc_int.Expr.eval (eval_int eval_other) expr)
    | Float_expr expr -> Float_value (Calc_float.Expr.eval (eval_float eval_other) expr)
    | To_float iexpr ->
      begin match eval eval_other iexpr with
        | Int_value i ->
          Float_value (Float.of_int i)
        | Float_value _ as fv ->
          fv
      end
    | To_int fexpr ->
      begin match eval eval_other fexpr with
        | Float_value f ->
          Int_value (Float.to_int f)
        | Int_value _ as iv ->
          iv
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
end

module Build =
struct
  let i i = Expr.Int_expr (Calc_int.Expr.Literal i)
  let f f = Expr.Float_expr (Calc_float.Expr.Literal f)

  let as_float = function
    | Expr.Float_expr fe -> fe
    | expr -> Calc_float.Expr.Other expr

  let as_int = function
    | Expr.Int_expr ie -> ie
    | expr -> Calc_int.Expr.Other expr

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
     | Expr.Float_value _, Expr.Int_value _
     | Expr.Int_value _, Expr.Float_value _ ->
       "*error: can't mix float and int expressions"
     | exception Failure str ->
       Printf.sprintf "*exception %s*" str)

let demo() =
  let i i = Expr.Int_value i in
  let f f = Expr.Float_value f in
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
  print_endline "ERROR CASES:";
  run (f 8.) Build.(f 3. + i 5);
  ()

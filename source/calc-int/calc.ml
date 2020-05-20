
module Type =
struct
  type t = Float | Int | Error of string [@@deriving show]
end

module Expr =
struct
  type binop =
    | Int_binop of Calc_int.Expr.binop
    | Float_binop of Calc_float.Expr.binop

  type t =
    | Int_expr of t Calc_int.Expr.t
    | Float_expr of t Calc_float.Expr.t
    | To_float of t
    | To_int of t
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
    | Error msg ->
      Type.Error msg

  let rec eval = function
    | Int_expr expr -> Int_value (Calc_int.Expr.eval eval_int expr)
    | Float_expr expr -> Float_value (Calc_float.Expr.eval eval_float expr)
    | To_float iexpr ->
      begin match eval iexpr with
        | Int_value i ->
          Float_value (Float.of_int i)
        | Float_value _ as fv ->
          fv
      end
    | To_int fexpr ->
      begin match eval fexpr with
        | Float_value f ->
          Int_value (Float.to_int f)
        | Int_value _ as iv ->
          iv
      end
    | Error str -> failwith str

  and eval_float expr =
    match eval expr with
    | Float_value f -> f
    | _ -> failwith "expected float"

  and eval_int expr =
    match eval expr with
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

  let ( + ) l r =
    match (Expr.type_of l, Expr.type_of r) with
    | Type.Int, Type.Int ->
      Expr.Int_expr (Calc_int.Expr.BinOp (as_int l, Calc_int.Expr.Add, as_int r))
    | Type.Float, Type.Float ->
      Expr.Float_expr (Calc_float.Expr.BinOp (as_float l, Calc_float.Expr.Add, as_float r))
    | _ ->
      Expr.Error "type error"

  let to_f e = Expr.To_float e
  let to_i e = Expr.To_int e
end

let run expect expr =
  Printf.printf "Running\n  %s\n  => %s\n"
    (Expr.show expr)
    (match (expect, Expr.eval expr) with
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
  run (f 8.) Build.(f 3. + f 5.);
  run (f 10.) Build.(to_f @@ i 8 + i 2);
  run (f 10.) Build.(f 8. + to_f (i 2));
  run (f 24.) Build.(to_f (i 16) + f 8.);
  run (i 8) Build.(to_i (f 8.));
  run (i 74) Build.(to_i (f 64.) + i 10);
  print_endline "ERROR CASES:";
  run (f 8.) Build.(f 3. + i 5);
  ()

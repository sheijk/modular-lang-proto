
module Type =
struct
  type t = Float | Int | Error of string [@@deriving show]
end

module Expr =
struct
  type binop =
    | Int_binop of Calc_int.Expr.binop
    | Float_binop of Calc_float.Expr.binop

  (* let to_function = function
   *   | Int_binop op -> Calc_int.Expr.to_function op
   *   | Float_binop op -> Calc_float.Expr.to_function op *)

  type t =
    | Int_expr of Calc_int.Expr.t
    | Float_expr of t Calc_float.Expr.t
    | To_float of t
    (* | To_int of t *)
    | Error of string
  [@@deriving show]

  type value =
    | Int_value of int
    | Float_value of float
  [@@deriving show]

  let type_of = function
    | Int_expr _ -> Type.Int
    | Float_expr _
    | To_float _ ->
      Type.Float
    | Error msg ->
      Type.Error msg

  let rec eval = function
    | Int_expr expr -> Int_value (Calc_int.Expr.eval expr)
    | Float_expr expr -> Float_value (Calc_float.Expr.eval eval_float expr)
    | To_float iexpr ->
      begin match eval iexpr with
        | Int_value i ->
          Float_value (Float.of_int i)
        | Float_value _ as fv ->
          fv
      end
    | Error str -> failwith str

  and eval_float expr =
    match eval expr with
    | Float_value f -> f
    | _ -> failwith "expected float"
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
    | _ -> failwith "can't embed into Calc_int"

  let ( + ) l r =
    match (Expr.type_of l, Expr.type_of r) with
    | Type.Int, Type.Int ->
      Expr.Int_expr (Calc_int.Expr.BinOp (as_int l, Calc_int.Expr.Add, as_int r))
    | Type.Float, Type.Float ->
      Expr.Float_expr (Calc_float.Expr.BinOp (as_float l, Calc_float.Expr.Add, as_float r))
    | _ ->
      Expr.Error "type error"

  let to_f e = Expr.To_float e
end

let run expr =
  Printf.printf "Running\n  %s\n  => %s\n"
    (Expr.show expr)
    (try Expr.eval expr |> Expr.show_value with
     | Failure str -> Printf.sprintf "*exception %s*" str)

let demo() =
  (* run @@ Build.(i 10 + to_f (i 2 * i 3)); *)
  run @@ Build.(i 10 + i 5);
  run @@ Build.(f 3. + f 5.);
  run @@ Build.(to_f @@ i 8 + i 2);
  run @@ Build.(f 8. + to_f (i 2));
  run @@ Build.(to_f (i 16) + f 8.);
  run @@ Build.(f 3. + i 5);
  ()

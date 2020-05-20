
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
    | Float_expr of Calc_float.Expr.t
    | To_float of t
    (* | To_int of t *)
    | Error of string
  [@@deriving show]

  type value =
    | Int_value of int
    | Float_value of float
  [@@deriving show]

  let rec eval = function
    | Int_expr expr -> Int_value (Calc_int.Expr.eval expr)
    | Float_expr expr -> Float_value (Calc_float.Expr.eval expr)
    | To_float iexpr ->
      begin match eval iexpr with
        | Int_value i ->
          Float_value (Float.of_int i)
        | Float_value _ as fv ->
          fv
      end
    | Error str -> failwith str
end

module Build =
struct
  let i i = Expr.Int_expr (Calc_int.Expr.Literal i)
  let f f = Expr.Float_expr (Calc_float.Expr.Literal f)

  let ( + ) l r =
    match (l, r) with
    | Expr.Int_expr li, Expr.Int_expr ri ->
      Expr.Int_expr (Calc_int.Expr.BinOp (li, Calc_int.Expr.Add, ri))
    | Expr.Float_expr lf, Expr.Float_expr rf ->
      Expr.Float_expr (Calc_float.Expr.BinOp (lf, Calc_float.Expr.Add, rf))
    (* This will not work because we can't embed the Calc_int expr inside a Calc_float one.
     * Also we now need to handle another case here instead of only checking the type.
     * | Expr.Float_expr lf, Expr.To_float ri ->
     *   Expr.Float_expr (Calc_float.Expr.BinOp (lf, Calc_float.Expr.Add, ri)) *)
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
  run @@ Build.(f 3. + i 5);
  run @@ Build.(to_f @@ i 8 + i 2);
  run @@ Build.(f 8. + to_f (i 2));
  ()

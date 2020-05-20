
module Type = Calc.Type

module Expr =
struct
  type t =
    | Int_expr of t Calc_int.Expr.t
    | If_expr of t * t * t
    | Error of string
  [@@deriving show]

  type value = Int_value of int [@@deriving show]

  let rec eval = function
    | Int_expr expr ->
      Int_value (Calc_int.Expr.eval eval_int expr)
    | If_expr (condition, true_body, false_body) ->
      begin match eval condition with
      | Int_value 0 -> eval false_body
      | _ -> eval true_body
      end
    | Error msg ->
      failwith msg

  and eval_int expr =
    match eval expr with
    | Int_value i -> i
    (* | _ -> failwith "expected int" *)
end

module Build =
struct
  let i i = Expr.Int_expr (Calc_int.Expr.Literal i)

  let make_binop op = fun l r ->
    Expr.Int_expr (Calc_int.Expr.BinOp (Calc_int.Expr.Other l, op, Calc_int.Expr.Other r))

  let ( + ) = make_binop Calc_int.Expr.Add
  let ( - ) = make_binop Calc_int.Expr.Sub

  let cond condition true_body false_body =
    Expr.If_expr (condition, true_body, false_body)
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
     | exception Failure str ->
       Printf.sprintf "*exception %s*" str)

let demo() =
  let i i = Expr.Int_value i in
  run (i 10) Build.(i 3 + i 7);
  run (i 2) Build.(cond (i 0) (i 1) (i 2));
  run (i 1) Build.(cond (i 1) (i 1) (i 2));
  run (i 22) Build.(cond (i 1 - i 1) (i 1 + i 10) (i 2 + i 20));
  ()


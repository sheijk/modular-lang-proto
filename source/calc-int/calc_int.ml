
module Expr =
struct
  type binop = Add | Sub | Mul | Div
  [@@deriving show]

  let to_function = function
    | Add -> ( + )
    | Sub -> ( - )
    | Mul -> ( * )
    | Div -> ( / )

  type t =
    | Literal of int
    | BinOp of t * binop * t
  [@@deriving show]

  let rec eval = function
    | Literal num -> num
    | BinOp (lhs, op, rhs) -> (to_function op) (eval lhs) (eval rhs)
end

module Build =
struct
  let c i = Expr.Literal i

  let ( + ) l r = Expr.BinOp (l, Expr.Add, r)
  let ( - ) l r = Expr.BinOp (l, Expr.Sub, r)
  let ( * ) l r = Expr.BinOp (l, Expr.Mul, r)
  let ( / ) l r = Expr.BinOp (l, Expr.Div, r)
end

let run expr =
  Printf.printf "Running\n  %s\n  => %d\n" (Expr.show expr) (Expr.eval expr)

let demo() =
  run @@ Build.(c 10 + c 2 * c 3);
  print_endline "Running calc-int demo"


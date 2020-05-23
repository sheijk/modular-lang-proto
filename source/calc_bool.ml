
module Expr =
struct
  type binop = And | Or [@@deriving show]

  let to_function = function
    | And -> ( && )
    | Or -> ( || )

  type 'a t =
    | Literal of bool
    | BinOp of 'a t * binop * 'a t
    | Other of 'a
  [@@deriving show]

  let rec eval eval_other = function
    | Literal b -> b
    | BinOp (lhs, op, rhs) -> (to_function op) (eval eval_other lhs) (eval eval_other rhs)
    | Other e -> eval_other e
end

module Build =
struct
  let b b = Expr.Literal b

  let ( && ) l r = Expr.BinOp (l, Expr.And, r)
  let ( || ) l r = Expr.BinOp (l, Expr.Or, r)
end

let show_void _ () = ()
let eval_void () = failwith "Cannot eval ()"

let run expr =
  Printf.printf "Running\n  %s\n  => %b\n" (Expr.show show_void expr) (Expr.eval eval_void expr)

let demo() =
  run @@ Build.(b true);
  run @@ Build.(b true && b true);
  run @@ Build.(b true && b false);
  run @@ Build.(b false || b true);
  ()

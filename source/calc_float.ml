
type binop = Add | Sub | Mul | Div
[@@deriving show]

let to_function = function
  | Add -> ( +. )
  | Sub -> ( -. )
  | Mul -> ( *. )
  | Div -> ( /. )

type 'a t =
  | Literal of float
  | BinOp of 'a t * binop * 'a t
  | Other of 'a
[@@deriving show]

let rec eval eval_other = function
  | Literal num -> num
  | BinOp (lhs, op, rhs) -> (to_function op) (eval eval_other lhs) (eval eval_other rhs)
  | Other e -> eval_other e

let simplify simplify (expr : _ t) =
  match expr with
  | Literal _ -> expr
  | BinOp (lhs, op, rhs) -> BinOp (simplify lhs, op, simplify rhs)
  | Other _ -> expr

module Build =
struct
  let c f = (Literal f : unit t)

  let ( + ) l r = BinOp (l, Add, r)
  let ( - ) l r = BinOp (l, Sub, r)
  let ( * ) l r = BinOp (l, Mul, r)
  let ( / ) l r = BinOp (l, Div, r)
end

let show_void _ () = ()
let eval_void () = failwith "Cannot eval ()"

let run (expr : unit t) =
  Printf.printf "Running\n  %s\n  => %f\n" (show show_void expr) (eval eval_void expr)

let demo() =
  run @@ Build.(c 10. + c 2. * c 3.);
  ()

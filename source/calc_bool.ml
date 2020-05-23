
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

let simplify (expr : _ t) = expr

module Build =
struct
  let b b = Literal b

  let ( && ) l r = BinOp (l, And, r)
  let ( || ) l r = BinOp (l, Or, r)
end

let show_void _ () = ()
let eval_void () = failwith "Cannot eval ()"

let run expr =
  Printf.printf "Running\n  %s\n  => %b\n" (show show_void expr) (eval eval_void expr)

let demo() =
  run @@ Build.(b true);
  run @@ Build.(b true && b true);
  run @@ Build.(b true && b false);
  run @@ Build.(b false || b true);
  ()

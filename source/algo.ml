
module Type =
struct
  type t = Int | Float | Bool | Error of string [@@deriving show]
end

module Expr =
struct
  type binop_bool = And | Or [@@deriving show]

  let to_function = function
    | And -> ( && )
    | Or -> ( || )

  type t =
    | Calc_expr of t Calc.Expr.t
    | If_expr of t * t * t
    | BoolLiteral of bool
    | BoolBinOp of t * binop_bool * t
    | Error of string
  [@@deriving show]

  type value =
    | Calc_value of Calc.Expr.value
    | Bool_value of bool
  [@@deriving show]

  let equal_value l r =
    match (l, r) with
    | Bool_value l, Bool_value r -> l = r
    | Calc_value (Calc.Expr.Int_value l), Calc_value (Calc.Expr.Int_value r) ->
      l = r
    | Calc_value (Calc.Expr.Float_value l), Calc_value (Calc.Expr.Float_value r) ->
      l = r
    | Calc_value (Calc.Expr.Float_value _), Calc_value (Calc.Expr.Int_value _)
    | Calc_value (Calc.Expr.Int_value _), Calc_value (Calc.Expr.Float_value _)
    | Bool_value _, Calc_value _
    | Calc_value _, Bool_value _ ->
      false

  let rec type_of = function
    | Calc_expr expr ->
      begin match Calc.Expr.type_of expr with
        | Calc.Type.Int -> Type.Int
        | Calc.Type.Float -> Type.Float
        | Calc.Type.Error msg -> Type.Error msg
      end
    | If_expr (_, l, r) ->
      begin match (type_of l, type_of r) with
        | Int, Int -> Int
        | Float, Float -> Float
        | _ -> Error "mismatching types"
      end
    | BoolLiteral _ ->
      Type.Bool
    | BoolBinOp (lhs, _, rhs) ->
      begin match (type_of lhs, type_of rhs) with
        | Type.Bool, Type.Bool -> Type.Bool
        | _ -> Type.Error "if branches must have the same type"
      end
    | Error msg ->
      Type.Error msg

  let rec eval = function
    | Calc_expr expr ->
      Calc_value (Calc.Expr.eval eval_calc expr)
    | If_expr (condition, true_body, false_body) ->
      begin match eval condition with
        | Bool_value false -> eval false_body
        | Bool_value true -> eval true_body
        | Calc_value (Calc.Expr.Int_value 0) -> eval false_body
        | Calc_value (Calc.Expr.Int_value _) -> eval true_body
        | Calc_value (Calc.Expr.Float_value 0.) -> eval false_body
        | Calc_value (Calc.Expr.Float_value _) -> eval true_body
      end
    | BoolLiteral b ->
      Bool_value b
    | BoolBinOp (lhs, op, rhs) ->
      begin match (eval lhs, eval rhs) with
        | Bool_value lhs, Bool_value rhs ->
          Bool_value ((to_function op) lhs rhs)
        | _ ->
          failwith "BoolBinOp needs two bool parameters"
      end
    | Error msg ->
      failwith msg

  and eval_calc expr =
    match eval expr with
    | Calc_value i -> i
    | Bool_value _ -> failwith "expected int"
end

module Build =
struct
  let i i : Expr.t = Expr.Calc_expr (Calc.Expr.Int_expr (Calc_int.Expr.Literal i))
  let f f : Expr.t = Expr.Calc_expr (Calc.Expr.Float_expr (Calc_float.Expr.Literal f))
  let b b : Expr.t = Expr.BoolLiteral b

  let make_binop op_int op_float = fun l r ->
    match (Expr.type_of l, Expr.type_of r) with
    | Type.Int, Type.Int ->
      Expr.Calc_expr (Calc.Expr.Int_expr (Calc_int.Expr.BinOp
                                            (Calc_int.Expr.Other (Calc.Expr.Other l),
                                             op_int,
                                             Calc_int.Expr.Other (Calc.Expr.Other r))))
    | Type.Float, Type.Float ->
      Expr.Calc_expr (Calc.Expr.Float_expr (Calc_float.Expr.BinOp
                                            (Calc_float.Expr.Other (Calc.Expr.Other l),
                                             op_float,
                                             Calc_float.Expr.Other (Calc.Expr.Other r))))
    | _ ->
      Expr.Error "type error"

  let ( + ) = make_binop Calc_int.Expr.Add Calc_float.Expr.Add
  let ( - ) = make_binop Calc_int.Expr.Sub Calc_float.Expr.Sub

  let ( && ) l r = Expr.BoolBinOp (l, Expr.And, r)
  let ( || ) l r = Expr.BoolBinOp (l, Expr.Or, r)

  let cond condition true_body false_body =
    Expr.If_expr (condition, true_body, false_body)
end

let had_errors = ref false

let run expect expr =
  Printf.printf "Running\n  %s\n  => %s\n"
    (Expr.show expr)
    (match Expr.eval expr with
     | result ->
       if Expr.equal_value expect result then
         Expr.show_value result
       else begin
         had_errors := true;
         Printf.sprintf "*error: expected %s but got %s" (Expr.show_value expect) (Expr.show_value result);
       end
     | exception Failure str ->
       had_errors := true;
       Printf.sprintf "*exception %s*" str)

let demo() =
  let i i = Expr.Calc_value (Calc.Expr.Int_value i) in
  let b b = Expr.Bool_value b in
  run (i 10) Build.(i 3 + i 7);
  run (i 2) Build.(cond (i 0) (i 1) (i 2));
  run (i 1) Build.(cond (i 1) (i 1) (i 2));
  run (i 22) Build.(cond (i 1 - i 1) (i 1 + i 10) (i 2 + i 20));
  run (i 11) Build.(cond (f 2. - f 1.) (i 1 + i 10) (i 2 + i 20));
  run (b true) Build.(b true || b false);
  run (b false) Build.(b true && b false);
  run (i 1) Build.(cond (b true) (i 1) (i 999));
  if !had_errors then
    print_endline "error: some tests failed";
  ()


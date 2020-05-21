
module Type =
struct
  type t = Int | Float | Bool | Error of string [@@deriving show]
end

module Expr =
struct
  type binop_bool = And | Or [@@deriving show]

  let to_function_bool = function
    | And -> ( && )
    | Or -> ( || )

  type comparison_t = Less | Greater | Equal [@@deriving show]

  let comparison_function = function
    | Less -> ( < )
    | Greater -> ( > )
    | Equal -> ( = )

  type t =
    | Calc_expr of t Calc.Expr.t
    | If_expr of t * t * t
    | BoolLiteral of bool
    | BoolBinOp of t * binop_bool * t
    | Comparison of t * comparison_t * t
    | Loop of t
    | LoopBreak of t
    | Error of string
  [@@deriving show]

  type value =
    | Calc_value of Calc.Expr.value
    | Bool_value of bool
  [@@deriving show]

  exception LoopBreakExn of value

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
    | Comparison _ ->
      Type.Bool
    | Loop expr -> type_of expr
    | LoopBreak expr -> type_of expr
    | Error msg ->
      Type.Error msg

  type context = unit

  let eval expr =
    let ctx = () in
    ignore ctx;
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
            Bool_value ((to_function_bool op) lhs rhs)
          | _ ->
            failwith "BoolBinOp needs two bool parameters"
        end
      | Comparison (lhs, op, rhs) ->
        begin match (eval lhs, eval rhs) with
          | Calc_value (Calc.Expr.Int_value lhs), Calc_value (Calc.Expr.Int_value rhs) ->
            Bool_value ((comparison_function op) lhs rhs)
          | Calc_value (Calc.Expr.Float_value lhs), Calc_value (Calc.Expr.Float_value rhs) ->
            Bool_value ((comparison_function op) lhs rhs)
          | _ ->
            failwith "Comparison needs two numeric parameters of the same type"
        end
      | Loop expr ->
        begin
          let rec loop index =
            if index > 100 then
              failwith "too many loop iterations";
            match eval expr with
            | Calc_value (Calc.Expr.Int_value 0) ->
              0
            | _ ->
              loop (index + 1)
          in
          try
            Calc_value (Calc.Expr.Int_value (loop 0))
          with
          | LoopBreakExn value -> value
        end
      | LoopBreak expr ->
        raise (LoopBreakExn (eval expr))
      | Error msg ->
        failwith msg

    and eval_calc expr =
      match eval expr with
      | Calc_value i -> i
      | Bool_value _ -> failwith "expected int"
    in
    eval expr
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

  let ( < ) l r = Expr.Comparison (l, Expr.Less, r)
  let ( > ) l r = Expr.Comparison (l, Expr.Greater, r)
  let ( = ) l r = Expr.Comparison (l, Expr.Equal, r)

  let cond condition true_body false_body =
    Expr.If_expr (condition, true_body, false_body)

  let loop expr =
    Expr.Loop expr
  let break expr =
    Expr.LoopBreak expr
end

let had_errors = ref false

let run expect expr =
  Printf.printf "Running\n  %s\n  => %s\n"
    (Expr.show expr)
    (match Expr.eval expr with
     | result ->
       let ok =
         match expect with
         | Some expect -> Expr.equal_value expect result
         | None -> false
       in
       if ok then
         Expr.show_value result
       else begin
         had_errors := true;
         let expect_str =
           match expect with
           | Some value -> Expr.show_value value
           | None -> "exception"
         in
         Printf.sprintf "*error: expected %s but got %s" expect_str (Expr.show_value result);
       end
     | exception Failure str ->
       let expected_result = expect = None in
       had_errors := not expected_result;
       Printf.sprintf "*exception %s*" str)

let demo() =
  let i i = Some (Expr.Calc_value (Calc.Expr.Int_value i)) in
  let b b = Some (Expr.Bool_value b) in
  let exn = None in

  run (i 10) Build.(i 3 + i 7);

  run (i 2) Build.(cond (i 0) (i 1) (i 2));
  run (i 1) Build.(cond (i 1) (i 1) (i 2));
  run (i 22) Build.(cond (i 1 - i 1) (i 1 + i 10) (i 2 + i 20));
  run (i 11) Build.(cond (f 2. - f 1.) (i 1 + i 10) (i 2 + i 20));
  run (i 1) Build.(cond (b true) (i 1) (i 999));

  run (b true) Build.(b true || b false);
  run (b false) Build.(b true && b false);

  run (b true) Build.(i 4 < i 10);
  run (b false) Build.(i 4 > i 10);
  run (b true) Build.(i 3 = i 3);
  run (b false) Build.(i 3 = i 4);
  run (b true) Build.(i 3 > i (-10));
  run (b false) Build.(i 3 > i 3);

  run exn Build.(loop (i 0));
  run (i 10) Build.(loop (break (i 10)));

  if !had_errors then
    print_endline "error: some tests failed";
  ()


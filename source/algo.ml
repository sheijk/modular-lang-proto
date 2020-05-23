
module Type =
struct
  type t = Int | Float | Bool | Error of string [@@deriving show]
end

module Expr =
struct
  type t =
    | Calc_expr of t Calc.Expr.t
    | If_expr of t * t * t
    | Loop of t
    | LoopBreak of t
    | LoopIndex
    | Error of string
  [@@deriving show]

  type value =
    | Calc_value of Calc.Expr.value
  [@@deriving show]

  exception LoopBreakExn of value

  let equal_value l r =
    match (l, r) with
    | Calc_value (Calc.Expr.Bool_value l), Calc_value (Calc.Expr.Bool_value r) ->
      l = r
    | Calc_value (Calc.Expr.Int_value l), Calc_value (Calc.Expr.Int_value r) ->
      l = r
    | Calc_value (Calc.Expr.Float_value l), Calc_value (Calc.Expr.Float_value r) ->
      l = r
    | Calc_value (Calc.Expr.Float_value _), Calc_value (Calc.Expr.Int_value _ | Calc.Expr.Bool_value _)
    | Calc_value (Calc.Expr.Int_value _), Calc_value (Calc.Expr.Float_value _ | Calc.Expr.Bool_value _)
    | Calc_value (Calc.Expr.Bool_value _), Calc_value (Calc.Expr.Float_value _ | Calc.Expr.Int_value _) ->
      false

  let rec type_of = function
    | Calc_expr expr ->
      begin match Calc.Expr.type_of expr with
        | Calc.Type.Bool -> Type.Float
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
    | Loop expr -> type_of expr
    | LoopBreak expr -> type_of expr
    | LoopIndex -> Type.Int
    | Error msg ->
      Type.Error msg

  type context = {
    index : int option;
  }

  let eval expr =
    let rec eval ctx =
      let eval_ctx ctx expr = eval ctx expr in
      let eval expr = eval ctx expr in
      let eval_calc expr = eval_calc ctx expr in
      function
      | Calc_expr expr ->
        Calc_value (Calc.Expr.eval eval_calc expr)
      | If_expr (condition, true_body, false_body) ->
        begin match eval condition with
          | Calc_value (Calc.Expr.Bool_value false) -> eval false_body
          | Calc_value (Calc.Expr.Bool_value true) -> eval true_body
          | Calc_value (Calc.Expr.Int_value 0) -> eval false_body
          | Calc_value (Calc.Expr.Int_value _) -> eval true_body
          | Calc_value (Calc.Expr.Float_value 0.) -> eval false_body
          | Calc_value (Calc.Expr.Float_value _) -> eval true_body
        end
      | Loop expr ->
        begin
          let rec loop index =
            if index > 100 then
              failwith "too many loop iterations";
            ignore (eval_ctx { index = Some index } expr);
            loop (index + 1)
          in
          try
            Calc_value (Calc.Expr.Int_value (loop 0))
          with
          | LoopBreakExn value -> value
        end
      | LoopBreak expr ->
        raise (LoopBreakExn (eval expr))
      | LoopIndex ->
        begin match ctx.index with
          | Some value -> Calc_value (Calc.Expr.Int_value value)
          | None -> failwith "index used outside of loop"
        end
      | Error msg ->
        failwith msg

    and eval_calc ctx expr =
      match eval ctx expr with
      | Calc_value i -> i
    in
    let ctx = { index = None } in
    eval ctx expr
end

module Build =
struct
  let i i : Expr.t = Expr.Calc_expr (Calc.Expr.Int_expr (Calc_int.Expr.Literal i))
  let f f : Expr.t = Expr.Calc_expr (Calc.Expr.Float_expr (Calc_float.Expr.Literal f))
  let b b : Expr.t = Expr.Calc_expr (Calc.Expr.Bool_expr (Calc_bool.Expr.Literal b))

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

  let to_bool (expr : Expr.t) : 'b Calc_bool.Expr.t =
    Calc_bool.Expr.Other (Calc.Expr.Other expr)

  let make_bool_op op = fun l r ->
    Expr.Calc_expr (Calc.Expr.Bool_expr (Calc_bool.Expr.BinOp (to_bool l, op, to_bool r)))

  let ( && ) = make_bool_op Calc_bool.Expr.And
  let ( || ) = make_bool_op Calc_bool.Expr.And

  let to_calc expr = (Calc.Expr.Other expr)

  let ( < ) l r = Expr.Calc_expr (Calc.Expr.Comparison (to_calc l, Calc.Expr.Less, to_calc r))
  let ( > ) l r = Expr.Calc_expr (Calc.Expr.Comparison (to_calc l, Calc.Expr.Greater, to_calc r))
  let ( = ) l r = Expr.Calc_expr (Calc.Expr.Comparison (to_calc l, Calc.Expr.Equal, to_calc r))

  let cond condition true_body false_body =
    Expr.If_expr (condition, true_body, false_body)

  let loop expr =
    Expr.Loop expr
  let break expr =
    Expr.LoopBreak expr
  let index =
    Expr.LoopIndex
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
  let b b = Some (Expr.Calc_value (Calc.Expr.Bool_value b)) in
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
  run (i 11) Build.(loop (cond (index > i 10) (break index) (i 1)));

  if !had_errors then
    print_endline "error: some tests failed";
  ()


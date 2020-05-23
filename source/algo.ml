
module Type =
struct
  type t = Int | Float | Bool | Error of string [@@deriving show]
end

type t =
  | Calc_expr of t Calc.t
  | If_expr of t * t * t
  | Loop of t
  | LoopBreak of t
  | LoopIndex
  | Error of string
[@@deriving show]

type value =
  | Calc_value of Calc.value
[@@deriving show]

exception LoopBreakExn of value

let equal_value l r =
  match (l, r) with
  | Calc_value (Calc.Bool_value l), Calc_value (Calc.Bool_value r) ->
    l = r
  | Calc_value (Calc.Int_value l), Calc_value (Calc.Int_value r) ->
    l = r
  | Calc_value (Calc.Float_value l), Calc_value (Calc.Float_value r) ->
    l = r
  | Calc_value (Calc.Float_value _), Calc_value (Calc.Int_value _ | Calc.Bool_value _)
  | Calc_value (Calc.Int_value _), Calc_value (Calc.Float_value _ | Calc.Bool_value _)
  | Calc_value (Calc.Bool_value _), Calc_value (Calc.Float_value _ | Calc.Int_value _) ->
    false

let rec type_of = function
  | Calc_expr expr ->
    begin match Calc.type_of expr with
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
      Calc_value (Calc.eval eval_calc expr)
    | If_expr (condition, true_body, false_body) ->
      begin match eval condition with
        | Calc_value (Calc.Bool_value false) -> eval false_body
        | Calc_value (Calc.Bool_value true) -> eval true_body
        | Calc_value (Calc.Int_value 0) -> eval false_body
        | Calc_value (Calc.Int_value _) -> eval true_body
        | Calc_value (Calc.Float_value 0.) -> eval false_body
        | Calc_value (Calc.Float_value _) -> eval true_body
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
          Calc_value (Calc.Int_value (loop 0))
        with
        | LoopBreakExn value -> value
      end
    | LoopBreak expr ->
      raise (LoopBreakExn (eval expr))
    | LoopIndex ->
      begin match ctx.index with
        | Some value -> Calc_value (Calc.Int_value value)
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

module Build =
struct
  let to_bool = function
    | Calc_expr (Calc.Bool_expr fexpr) -> fexpr
    | expr -> Calc_bool.Other (Calc.Other expr)
  let to_int = function
    | Calc_expr (Calc.Int_expr fexpr) -> fexpr
    | expr -> Calc_int.Other (Calc.Other expr)
  let to_float = function
    | Calc_expr (Calc.Float_expr fexpr) -> fexpr
    | expr -> Calc_float.Other (Calc.Other expr)
  let to_calc = function
    | Calc_expr cexpr -> cexpr
    | expr -> Calc.Other expr

  let from_bool bexpr = Calc_expr (Calc.Bool_expr bexpr)
  let from_int iexpr = Calc_expr (Calc.Int_expr iexpr)
  let from_float iexpr = Calc_expr (Calc.Float_expr iexpr)
  let from_calc cexpr = Calc_expr cexpr

  let b b : t = from_bool (Calc_bool.Literal b)
  let i i : t = from_int (Calc_int.Literal i)
  let f f : t = from_float (Calc_float.Literal f)

  let make_binop op_int op_float = fun l r ->
    match (type_of l, type_of r) with
    | Type.Int, Type.Int ->
      from_int (Calc_int.BinOp (to_int l, op_int, to_int r))
    | Type.Float, Type.Float ->
      from_float (Calc_float.BinOp (to_float l, op_float, to_float r))
    | _ ->
      Error "type error"

  let ( + ) = make_binop Calc_int.Add Calc_float.Add
  let ( - ) = make_binop Calc_int.Sub Calc_float.Sub
  let ( * ) = make_binop Calc_int.Mul Calc_float.Mul
  let ( / ) = make_binop Calc_int.Div Calc_float.Div

  let make_bool_op op = fun l r ->
    from_bool (Calc_bool.BinOp (to_bool l, op, to_bool r))

  let ( && ) = make_bool_op Calc_bool.And
  let ( || ) = make_bool_op Calc_bool.And

  let ( < ) l r = from_calc (Calc.Comparison (to_calc l, Calc.Less, to_calc r))
  let ( > ) l r = from_calc (Calc.Comparison (to_calc l, Calc.Greater, to_calc r))
  let ( = ) l r = from_calc (Calc.Comparison (to_calc l, Calc.Equal, to_calc r))

  let cond condition true_body false_body =
    If_expr (condition, true_body, false_body)

  let loop expr = Loop expr
  let break expr = LoopBreak expr
  let index = LoopIndex
end

let had_errors = ref false

let run expect expr =
  Printf.printf "Running\n  %s\n  => %s\n"
    (show expr)
    (match eval expr with
     | result ->
       let ok =
         match expect with
         | Some expect -> equal_value expect result
         | None -> false
       in
       if ok then
         show_value result
       else begin
         had_errors := true;
         let expect_str =
           match expect with
           | Some value -> show_value value
           | None -> "exception"
         in
         Printf.sprintf "*error: expected %s but got %s" expect_str (show_value result);
       end
     | exception Failure str ->
       let expected_result = expect = None in
       had_errors := not expected_result;
       Printf.sprintf "*exception %s*" str)

let demo() =
  let i i = Some (Calc_value (Calc.Int_value i)) in
  let b b = Some (Calc_value (Calc.Bool_value b)) in
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


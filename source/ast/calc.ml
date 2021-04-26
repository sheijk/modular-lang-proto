
module Type =
struct
  type t = Float | Int | Bool | Error of string [@@deriving show]
end

type comparison_t = Less | Greater | Equal [@@deriving show]

let comparison_function = function
  | Less -> ( < )
  | Greater -> ( > )
  | Equal -> ( = )

type 'a t =
  | Int_expr of 'a t Calc_int.t
  | Float_expr of 'a t Calc_float.t
  | Bool_expr of 'a t Calc_bool.t
  | To_float of 'a t
  | To_int of 'a t
  | Comparison of 'a t * comparison_t * 'a t
  | Other of 'a
  | Error of string
[@@deriving show]

type value =
  | Int_value of int
  | Float_value of float
  | Bool_value of bool
[@@deriving show]

let equal_value l r =
  match (l, r) with
  | (Bool_value l), (Bool_value r) ->
    l = r
  | (Int_value l), (Int_value r) ->
    l = r
  | (Float_value l), (Float_value r) ->
    l = r
  | (Float_value _), (Int_value _ | Bool_value _)
  | (Int_value _), (Float_value _ | Bool_value _)
  | (Bool_value _), (Float_value _ | Int_value _) ->
    false

let type_of = function
  | Int_expr _
  | To_int _ ->
    Type.Int
  | Float_expr _
  | To_float _ ->
    Type.Float
  | Bool_expr _
  | Comparison _ ->
    Type.Bool
  | Other _ ->
    Type.Error "invalid embedded type"
  | Error msg ->
    Type.Error msg

let rec eval eval_other = function
  | Int_expr expr ->
    Int_value (Calc_int.eval (eval_int eval_other) expr)
  | Float_expr expr ->
    Float_value (Calc_float.eval (eval_float eval_other) expr)
  | Bool_expr expr ->
    Bool_value (Calc_bool.eval (eval_bool eval_other) expr)
  | To_float iexpr ->
    begin match eval eval_other iexpr with
      | Int_value i ->
        Float_value (Float.of_int i)
      | Float_value _ as fv ->
        fv
      | Bool_value b ->
        Float_value (if b then 1.0 else 0.0)
    end
  | To_int fexpr ->
    begin match eval eval_other fexpr with
      | Float_value f ->
        Int_value (Float.to_int f)
      | Int_value _ as iv ->
        iv
      | Bool_value b ->
        Int_value (if b then 1 else 0)
    end
  | Comparison (lhs, op, rhs) ->
    begin match (eval eval_other lhs, eval eval_other rhs) with
      | Int_value lhs, Int_value rhs ->
        Bool_value ((comparison_function op) lhs rhs)
      | Float_value lhs, Float_value rhs ->
        Bool_value ((comparison_function op) lhs rhs)
      | _ ->
        failwith "Comparison needs two numeric parameters of the same type"
    end
  | Other expr ->
    eval_other expr
  | Error str ->
    failwith str

and eval_float eval_other expr =
  match eval eval_other expr with
  | Float_value f -> f
  | _ -> failwith "expected float"

and eval_int eval_other expr =
  match eval eval_other expr with
  | Int_value i -> i
  | _ -> failwith "expected int"

and eval_bool eval_other expr =
  match eval eval_other expr with
  | Bool_value b -> b
  | _ -> failwith "expected bool"

module Build =
struct
  let to_bool = function
    | Bool_expr ie -> ie
    | expr -> Calc_bool.Other expr

  let to_int = function
    | Int_expr ie -> ie
    | expr -> Calc_int.Other expr

  let to_float = function
    | Float_expr fe -> fe
    | expr -> Calc_float.Other expr

  let from_bool bexpr = Bool_expr bexpr
  let from_int iexpr = Int_expr iexpr
  let from_float fexpr = Float_expr fexpr

  let b b = from_bool (Calc_bool.Literal b)
  let i i = from_int (Calc_int.Literal i)
  let f f = from_float (Calc_float.Literal f)

  let make_num_op op_int op_float = fun l r ->
    match (type_of l, type_of r) with
    | Type.Int, Type.Int ->
      from_int (Calc_int.BinOp (to_int l, op_int, to_int r))
    | Type.Float, Type.Float ->
      from_float (Calc_float.BinOp (to_float l, op_float, to_float r))
    | _ ->
      Error "type error"

  let ( + ) = make_num_op Calc_int.Add Calc_float.Add
  let ( - ) = make_num_op Calc_int.Sub Calc_float.Sub
  let ( * ) = make_num_op Calc_int.Mul Calc_float.Mul
  let ( / ) = make_num_op Calc_int.Div Calc_float.Div

  let ( && ) l r = from_bool (Calc_bool.BinOp (to_bool l, Calc_bool.And, to_bool r))
  let ( || ) l r = from_bool (Calc_bool.BinOp (to_bool l, Calc_bool.Or, to_bool r))

  let ( < ) l r = Comparison (l, Less, r)
  let ( > ) l r = Comparison (l, Greater, r)
  let ( = ) l r = Comparison (l, Equal, r)

  let to_f e = To_float e
  let to_i e = To_int e
end

let show_void _ () = ()
let eval_void () = failwith "Cannot eval ()"

let run expect expr =
  Printf.printf "Running\n  %s\n  => %s\n"
    (show show_void expr)
    (match (expect, eval eval_void expr) with
     | Int_value expect, (Int_value got as gotv) ->
       let value_str = show_value gotv in
       if expect = got then
         value_str
       else
         Printf.sprintf "*error: expected %d but got %s" expect value_str
     | Float_value expect, (Float_value got as gotv) ->
       let value_str = show_value gotv in
       if expect -. got < epsilon_float then
         value_str
       else
         Printf.sprintf "*error: expected %f but got %s" expect value_str
     | Bool_value expect, (Bool_value got as gotv) ->
       let value_str = show_value gotv in
       if expect = got then
         value_str
       else
         Printf.sprintf "*error: expected %b but got %s" expect value_str
     | Float_value _, (Int_value _ | Bool_value _)
     | Int_value _, (Float_value _ | Bool_value _)
     | Bool_value _, (Int_value _ | Float_value _) ->
       "*error: can't mix float and int expressions"
     | exception Failure str ->
       Printf.sprintf "*exception %s*" str)

let demo() =
  let i i = Int_value i in
  let f f = Float_value f in
  let b b = Bool_value b in

  (* run @@ Build.(i 10 + to_f (i 2 * i 3)); *)
  run (i 15) Build.(i 10 + i 5);
  run (f 8.) Build.(f 13. - f 5.);
  run (i 123) Build.((i 1 * i 10 + i 2) * i 10 + i 3);
  run (i 5) Build.(i 10 / i 2);

  run (f 10.) Build.(to_f @@ i 8 + i 2);
  run (f 10.) Build.(f 8. + to_f (i 2));
  run (f 24.) Build.(to_f (i 16) + f 8.);
  run (i 8) Build.(to_i (f 8.));
  run (i 74) Build.(to_i (f 64.) + i 10);

  run (b true) Build.(i 4 < i 10);
  run (b false) Build.(i 4 > i 10);
  run (b true) Build.(i 3 = i 3);
  run (b false) Build.(i 3 = i 4);
  run (b true) Build.(i 3 > i (-10));
  run (b false) Build.(i 3 > i 3);

  run (b true) Build.(b true || b false);
  run (b false) Build.(b true && b false);

  print_endline "ERROR CASES:";
  run (f 8.) Build.(f 3. + i 5);
  ()

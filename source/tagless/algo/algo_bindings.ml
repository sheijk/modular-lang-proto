module type Lang =
sig
  type t
  include Calc.Lang with type t := t
  include Algo.Lang with type t := t
  include Bindings.Lang with type t := t
  include Locations.Lang with type t := t
end

module Tests(L : Lang) =
struct
  type t = L.t
  type value = Interpreter.Default_values.value
  module I = Interpreter.Dynamic

  let is_int n = Some (I.int n)
  let is_bool b = Some (I.bool b)

  let tests =
    let module C = Algo_calc.Tests(L) in
    C.tests @ L.[
        is_int 99, let_ "foo" (int 99) (get "foo");
        is_int 123,
        let_ "foo" (int (-1))
          (get "foo" +.
           let_ "foo" (int 24)
             (int 100 +. get "foo"));

        is_int 14,
        at (file "Algo_bindings.Tests.ml") ~line:10 ~column:8 @@
        let_ "sum" (int 0)
          (loop
             (set "sum" (loop_index() *. loop_index() +. get "sum")
                (if_ (get "sum" >. int 10)
                   (break (get "sum"))
                   (int 1))))
      ]
end

module Tests_compiler_errors(L : Lang) =
struct
  type t = L.t
  type value = Interpreter.Default_values.value

  let is_int n = Some (Interpreter.Default_values.int n)
  let is_bool b = Some (Interpreter.Default_values.bool b)

  let tests =
    let module B = Tests(L) in
    B.tests @ L.[
        None, if_ (bool false) (loop_index()) (int 0);
        None,
        let_ "x" (int 10)
          (get "x" +. get "y")
      ]
end

module Tests_optimize(L : Lang) =
struct
  type t = L.t
  type value = Interpreter.Default_values.value

  let is_int n = Some (Interpreter.Default_values.int n)
  let is_bool b = Some (Interpreter.Default_values.bool b)
  let fails = None

  let tests = L.[
      is_bool true, bool true;
      is_bool false, bool false;

      is_bool true, bool false || bool true;
      is_bool true, bool true || bool false;
      is_bool false, bool false || bool false;
      is_bool true, bool true && bool true;
      is_bool false, bool true && bool false;

      is_bool true, bool true && (bool true || bool false);
      is_bool false, bool true && bool false || bool false && bool true;

      is_bool false, int 3 =. int 4;
      is_bool false, int 3 >. int 3;
      is_bool false, int 4 >. int 10;
      is_bool true, int 3 =. int 3;
      is_bool true, int 3 >. int (-10);
      is_bool true, int 4 <. int 10;

      is_int 1, int 1;
      is_int 5, int 10 /. int 2;
      is_int 10, int 3 +. int 7;
      is_int 15,
      at (file "Tests_algo_optimize.ml") ~line:10 ~column:8 @@
      int 10 +. int 5;

      is_int 123, (int 1 *. int 10 +. int 2) *. int 10 +. int 3;

      is_int 3, if_ (int 10 >. int 20) (int 666) (int 3);
      fails, loop (int 0);
      fails, loop (int 1);
      fails, break (int 3);
      fails, loop (break (int 10));
      fails, loop (if_ (loop_index() >. int 10) (break @@ loop_index()) (int 1));
      is_int 2, if_ (bool false) (int 1) (int 2);
      is_int 1, if_ (bool true) (int 1) (int 2);
      is_int 22, if_ (int 1 -. int 1 >. int 1) (int 1 +. int 10) (int 2 +. int 20);
      is_int 11, if_ (int 2 -. int 1 <. int 2) (int 1 +. int 10) (int 2 +. int 20);
      is_int 1, if_ (bool true) (int 1) (int 999);

      fails, if_ (int 0 >. (loop (break @@ int 1))) (int 5) (int 3 +. int 2);
      fails, if_ (int 0 >. (loop @@ int 0)) (int 1) (int 1);

      fails, let_ "foo" (int 99) (get "foo");
      fails, let_ "foo" (int 99) (int 1 +. get "foo");
      fails,
      let_ "foo" (int (-1))
        (get "foo" +.
         let_ "foo" (int 24)
           (int 100 +. get "foo"));

      fails,
      let_ "sum" (int 0)
        (loop
           (set "sum" (loop_index() *. loop_index() +. get "sum")
              (if_ (get "sum" >. int 10)
                 (break (get "sum"))
                 (int 1))))
    ]
end

module To_st(S : Strlang.Lang) =
struct
  include Empty.To_st(S)
  include Calc.To_st(S)
  include Algo.To_st(S)
  include Bindings.To_st(S)
  include Locations.To_st(S)
end
module To_string = To_st(Strlang.To_string)
let () = let module T : Algo.Lang = To_string in ()

module Eval(I : Interpreter.All) =
struct
  include Empty.Eval(I)
  include Calc.Eval(I)
  include Algo.Eval(I)
  include Bindings.Eval(I)
  include Locations.Eval(I)
end
let () = let module T : Algo.Lang = Eval(Interpreter.Dynamic) in ()

module Eval_compiled =
struct
  include Empty.Eval_compiled
  include Calc.Eval_compiled
  include Algo.Eval_compiled
  include Bindings.Eval_compiled
  include Locations.Eval_compiled
end
let () = let module T : Lang = Eval_compiled in ()

module Count_ast_size =
struct
  include Empty.Count_ast_size
  include Calc.Count_ast_size
  include Algo.Count_ast_size
  include Bindings.Count_ast_size
end
(* let () = let module T : Lang = Count_ast_size in () *)

module Optimize(L : Lang) =
struct
  include Empty.Optimize(L)
  include Calc.Optimize(L)
  include Algo.Optimize(L)
  include Bindings.Optimize(L)
  include Locations.Optimize(L)
end
let () = let module T : Lang = Optimize(To_string) in ()

module Parse_rules(L : Lang) : (Parser.Rules with type t = L.t) =
struct
  include Empty.Parse_rules(L)

  let readers =
    let module C = Calc.Parse_rules(L) in
    let module A = Algo.Parse_rules(L) in
    let module B = Bindings.Parse_rules(L) in
    let module Loc = Locations.Parse_rules(L) in
    C.readers @
    A.readers @
    B.readers @
    Loc.readers @
    []
end

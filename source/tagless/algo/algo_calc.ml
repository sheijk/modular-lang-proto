module type Lang =
sig
  type t
  include Calc.Lang with type t := t
  include Algo.Lang with type t := t
  include Locations.Lang with type t := t
end

module Tests(L : Lang) =
struct
  type t = L.t
  module I = Interpreter.Dynamic
  type value = Interpreter.Default_values.value
  type expected = value option
  type interpreter = I.t

  let is_int n = Some (I.int n)
  let is_bool b = Some (I.bool b)

  let tests =
    let module C = Calc.Tests(L) in
    C.tests @ L.[
        is_int 10, int 3 +. int 7;
        is_int 5, int 10 /. int 2;
        is_int 3, if_ (int 10 >. int 20) (int 666) (int 3);
        None, (loop (int 0));
        None, (break (int 3));
        is_int 10, (loop (break (int 10)));
        is_int 11, (loop (if_ (loop_index() >. int 10) (break @@ loop_index()) (int 1)));
        None, loop (int 1);
        is_int 2, (if_ (bool false) (int 1) (int 2));
        is_int 1, (if_ (bool true) (int 1) (int 2));
        is_int 22, (if_ (int 1 -. int 1 >. int 1) (int 1 +. int 10) (int 2 +. int 20));
        is_int 11, (if_ (int 2 -. int 1 <. int 2) (int 1 +. int 10) (int 2 +. int 20));
        is_int 1, (if_ (bool true) (int 1) (int 999));

        is_bool true, bool true;
      ]
end

module To_st(S : Strlang.Lang) =
struct
  include Empty.To_st(S)
  include Calc.To_st(S)
  include Algo.To_st(S)
  include Locations.To_st(S)
end
module To_string = To_st(Strlang.To_string)
let () = let module T : Algo.Lang = To_string in ()
let () = let module T : Empty.Test_cases_eval = Tests(To_string) in ()

module Eval(I : Interpreter.Loop) =
struct
  include Empty.Eval(I)
  include Calc.Eval(I)
  include Algo.Eval(I)
  include Locations.Eval(I)
end
let () = let module T : Algo.Lang = Eval(Interpreter.Dynamic) in ()

module Eval_compiled =
struct
  include Empty.Eval_compiled
  include Calc.Eval_compiled
  include Algo.Eval_compiled
  include Locations.Eval_compiled
end
let () = let module T : Lang = Eval_compiled in ()

module Parse_rules(L : Lang) : (Parser.Rules with type t = L.t) =
struct
  include Empty.Parse_rules(L)

  let readers =
    let module C = Calc.Parse_rules(L) in
    let module A = Algo.Parse_rules(L) in
    let module Loc = Locations.Parse_rules(L) in
    C.readers @
    A.readers @
    Loc.readers @
    [
      "=.", Parser.binop L.( =. );
      "<.", Parser.binop L.( <. );
      ">.", Parser.binop L.( >. );
    ]
end

module type Lang =
sig
  type t
  include Calc.Lang with type t := t
  include Algo.Lang with type t := t
end

module To_st(S : Strlang.Lang) =
struct
  include Empty.To_st(S)
  include Calc.To_st(S)
  include Algo.To_st(S)
end
module To_string = To_st(Strlang.To_string)
let () = let module T : Algo.Lang = To_string in ()

module Eval(I : Interpreter.Loop) =
struct
  include Empty.Eval(I)
  include Calc.Eval(I)
  include Algo.Eval(I)
end
let () = let module T : Algo.Lang = Eval(Interpreter.Dynamic) in ()

module Eval_compiled =
struct
  include Empty.Eval_compiled
  include Calc.Eval_compiled
  include Algo.Eval_compiled
end
let () = let module T : Lang = Eval_compiled in ()

module Parse_rules(L : Lang) : (Parser.Rules with type t = L.t) =
struct
  include Empty.Parse_rules(L)

  let readers =
    let module C = Calc.Parse_rules(L) in
    let module A = Algo.Parse_rules(L) in
    C.readers @
    A.readers @
    [
      "=.", Parser.binop L.( =. );
      "<.", Parser.binop L.( <. );
      ">.", Parser.binop L.( >. );
    ]
end

module type Lang =
sig
  type t
  include Calc.Lang with type t := t
  include Algo.Lang with type t := t
  include Bindings.Lang with type t := t
  include Locations.Lang with type t := t
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

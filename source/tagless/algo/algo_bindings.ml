module type Lang =
sig
  type 'a t
  include Calc.Lang with type 'a t := 'a t
  include Algo.Lang with type 'a t := 'a t
  include Bindings.Lang with type 'a t := 'a t
end

module To_string =
struct
  include Empty.To_string
  include Calc.To_string
  include Algo.To_string
  include Bindings.To_string
end
let () = let module T : Algo.Lang = To_string in ()

module Eval(I : Interpreter.All) =
struct
  include Empty.Eval(I)
  include Calc.Eval(I)
  include Algo.Eval(I)
  include Bindings.Eval(I)
end
let () = let module T : Algo.Lang = Eval(Interpreter.Dynamic) in ()

module Eval_compiled =
struct
  include Empty.Eval_compiled
  include Calc.Eval_compiled
  include Algo.Eval_compiled
  include Bindings.Eval_compiled
end
let () = let module T : Lang = Eval_compiled in ()

module Count_ast_size =
struct
  include Empty.Count_ast_size
  include Calc.Count_ast_size
  include Algo.Count_ast_size
  include Bindings.Count_ast_size
end
let () = let module T : Lang = Count_ast_size in ()

module Optimize(L : Lang) =
struct
  include Empty.Optimize(L)
  include Calc.Optimize(L)
  include Algo.Optimize(L)
  include Bindings.Optimize(L)
end
let () = let module T : Lang = Optimize(Count_ast_size) in ()

module String_tree =
struct
  type t =
    | Leaf of string
    | Tree of t list

  module Build =
  struct
    let s str = Leaf str
    let t children = Tree children

    let test =
      t [s "int"; s "7"]
  end
end

module Attempt1 =
struct
  module type Parsable =
  sig
    type 'a t
    val readers : (string * (String_tree.t list -> bool t option)) list
  end

  module Parse(P : Parsable) =
  struct
    let parse tree =
      let translate hd args =
        try
          let _, parse = List.find (fun (handled, _) -> handled = hd) P.readers in
          parse args
        with Failure _ ->
          failwith "unknown language form"
      in
      match tree with
      | String_tree.Tree (String_tree.Leaf hd :: args) ->
        translate hd args
      | _ -> failwith "expected a tree"
  end

  module Parse_bool(L : Calc_bool.Lang) : Parsable =
  struct
    type 'a t = 'a L.t

    let bool = function
      | [String_tree.Leaf "true"] -> Some (L.bool true)
      | [String_tree.Leaf "false"] -> Some (L.bool true)
      | _ -> None

    let readers = [
      "bool", bool;
      (* "&&", and_; *)
      (* "||", or_; *)
    ]
  end
end


module Attempt2 =
struct
end

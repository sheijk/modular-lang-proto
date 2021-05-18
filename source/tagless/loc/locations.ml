module type Lang =
sig
  type t
  type compilation_unit

  val file : string -> compilation_unit
  val at : compilation_unit -> line:int -> column:int -> t -> t
end

module File_compilation_unit =
struct
  type compilation_unit = string
  let file path = path
end

module To_st(L : Strlang.Lang) : (Lang with type t = L.t) =
struct
  include Empty.To_st(L)
  include File_compilation_unit

  let at file ~line ~column t =
    L.tree [
      L.leaf "at";
      L.tree [
        L.leaf file;
        L.leaf (string_of_int line);
        L.leaf (string_of_int column);
      ];
      t
    ]
end
module To_string = To_st(Strlang.To_string)
let () = let module T : Lang = To_string in ()

module Eval(I : Interpreter.Values) =
struct
  include Empty.Eval(I)
  include File_compilation_unit

  let at _file ~line:_ ~column:_ t = t
end
let () = let module T : Lang = Eval(Interpreter.No_runtime) in ()

module Eval_compiled =
struct
  include Empty.Eval_compiled
  include File_compilation_unit

  let at file ~line ~column (info, t) =
    let loc = Compiler.Info.location file line column in
    Compiler.Info.at loc info, t
end
let () = let module T : Lang = Eval_compiled in ()

module Optimize(L : Lang) =
struct
  include Empty.Optimize(L)
  include File_compilation_unit

  let at _file ~line:_ ~column:_ t = t
end
let () = let module T : Lang = Optimize(To_string) in ()

module Parse_rules(L : Lang) : (Parser.Rules with type t = L.t) =
struct
  include Empty.Parse_rules(L)

  let readers =
    Strlang.Tree.[
      "at", (fun _parse_rec parse st ->
          match st with
          | Tree [_; Tree [Leaf filename; Leaf line_str; Leaf column_str]; t] ->
            L.at
              (L.file filename)
              ~line:(int_of_string line_str)
              ~column:(int_of_string column_str)
              (parse t)
          | st -> Parser.parse_error_at "invalid location " st);
    ]
end

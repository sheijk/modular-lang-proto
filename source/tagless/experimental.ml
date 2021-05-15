module type String_lang =
sig
  type t
  val leaf : string -> t
  val tree : t list -> t
end

module String_tree =
struct
  type t =
    | Leaf of string
    | Tree of t list

  let leaf str = Leaf str
  let tree children = Tree children

  let rec to_string = function
    | Leaf s -> s
    | Tree children ->
      let children_str = List.map to_string children in
      "[" ^ String.concat ", " children_str ^ "]"

  module Build =
  struct
    let s str = Leaf str
    let t children = Tree children

    let test =
      t [s "int"; s "7"]
  end
end
let () = let module M : String_lang = String_tree in ()

module Attempt1 =
struct
  module St = String_tree

  module Parse_bool(L : Calc_bool.Lang) =
  struct
    type 'a t = 'a L.t

    let rec parse_any readers = function
      | St.Leaf f :: args ->
        let _, read = List.find (fun (name, _) -> name = f) readers in
        read (parse_any readers) args
      | [St.Tree args] ->
        parse_any readers args
      | _ ->
        failwith "parse"

    let map2 f a b =
      match a, b with
      | Some a, Some b -> f a b
      | _ -> None

    let bool _ = function
      | [St.Leaf "true"] -> Some (L.bool true)
      | [St.Leaf "false"] -> Some (L.bool false)
      | _ -> None

    let and_ parse_arg = function
      | [lhs; rhs] ->
        map2
          (fun lhs rhs -> Some L.(lhs && rhs))
          (parse_arg [lhs])
          (parse_arg [rhs])
      | _ ->
        None

    let readers = [
      "bool", bool;
      "&&", and_;
    ]

    let parse = parse_any readers
  end

  (* module Parse(L : Calc_bool.Lang) : String_lang =
   * struct
   *   type 'a t = String of string | L of 'a L.t
   * 
   *   let leaf str = String str
   *   let tree = function
   *     | [String "bool"; String "true"] -> L (L.bool true)
   *     | [String "bool"; String "false"] -> L (L.bool false)
   *     | [String "&&"; L lhs; L rhs] -> L L.(lhs && rhs)
   *     | _ -> (failwith "syntax error" : unit -> int t)
   * end *)

  module Test(L : Calc_bool.Lang) =
  struct
    let a = L.(bool true && bool false)
  end

  let test_cases = St.[
      [leaf "bool"; leaf "true"];
      [leaf "bool"; leaf "false"];
      [leaf "&&";
       tree [leaf "bool"; leaf "true"];
       tree [leaf "bool"; leaf "false"]]
    ]

  let test () =
    print_endline "Testing Experimental.Attempt1 parsing";
    let module P = Parse_bool(Calc_bool.To_string) in
    let test st =
      let to_string st = String_tree.to_string (Tree st) in
      match P.parse st with
      | Some str ->
        Printf.printf "  %s -> %s\n" (to_string st) str
      | None ->
        Printf.printf "  %s -> syntax error\n" (to_string st)
    in
    List.iter test test_cases
end

module Module =
struct
  module type Lang =
  sig
    type type_
    type rule
    type t

    val unit : type_
    val int : type_
    val bool : type_
    val t : type_ -> type_

    val rule : string -> (string * type_) list -> type_ -> rule
    val include_ : t -> rule

    val lang : string -> rule list -> t
  end

  module Test_cases(L : Lang) =
  struct
    let types = L.[
        int;
        bool;
        t int;
        t bool;
      ]

    let rules = L.[
        rule "int" ["v", int] @@ t int;
        rule "bool" ["v", bool] @@ t bool;
        rule "num" ["negative", t bool; "value", t int] @@ t int;
      ]

    let calc_bool = L.lang "Calc_bool" L.[
        rule "bool" ["v", bool] @@ t bool;
        rule "&&" ["l", bool; "r", bool] @@ t bool;
        rule "||" ["l", bool; "r", bool] @@ t bool;
      ]

    let algo = L.lang "Algo" L.[
        rule "int" ["v", int] @@ t int;
        rule "if" ["condition", t bool; "true", t int; "false", t int] @@ t int;
        rule "loop" ["body", t int] @@ t int;
        rule "break" ["value", t int] @@ t int;
        rule "loop_index" ["_", unit] @@ t int;
      ]

    let langs = L.[
        calc_bool;
        algo;

        lang "Algo_calc" [
          include_ calc_bool;
          include_ algo;
        ];
      ]
  end
end

let test () =
  Attempt1.test()

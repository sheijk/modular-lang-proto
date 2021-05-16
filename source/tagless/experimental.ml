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
    type t = L.t

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

module Attempt2 =
struct
  module Parse(L : Calc_bool.Lang) =
  struct
    type t =
      | Int of int
      | Bool of bool
      | String of string
      | Node of L.t

    type observed = L.t

    let observe = function
      | Node n -> n
      | _ -> failwith "observe"

    let string_each f str =
      let rec check i =
        if i >= String.length str then
          true
        else if f str.[i] then
          check (i+1)
        else
          false
      in
      check 0

    let is_number str = string_each (fun c -> c >= '0' && c <= '9') str

    let leaf = function
      | "true" -> Bool true
      | "false" -> Bool false
      | str when is_number str -> Int (int_of_string str)
      | str -> String str

    let tree = function
      | [String "bool"; Bool b] -> Node (L.bool b)
      | [String "&&"; Node l; Node r] -> Node L.(l && r)
      | [String "||"; Node l; Node r] -> Node L.(l || r)
      | _ -> failwith "invalid tree"
  end
  let () = let module T : String_lang = Parse(Calc_bool.To_string) in ()

  module Test_cases(St : String_lang) =
  struct
    let test_cases = St.[
        tree [leaf "bool"; leaf "true"];
        tree [leaf "bool"; leaf "false"];
        tree [leaf "&&";
         tree [leaf "bool"; leaf "true"];
         tree [leaf "bool"; leaf "false"]];
        tree [leaf "||";
              tree [leaf "bool"; leaf "false"];
              tree [leaf "bool"; leaf "true"]]
      ]
  end

  let test () =
    print_endline "Testing Experimental.Attempt2 parsing";
    let module P = Parse(Calc_bool.To_string) in
    let module Results = Test_cases(P) in
    let module Cases = Test_cases(String_tree) in
    let to_string st = String_tree.to_string st in
    let test st result =
      try
        let result = P.observe result in
        Printf.printf "  %s -> %s\n" (to_string st) result
      with Failure str ->
        Printf.printf "  %s -> error %s" (to_string st) str
    in
    List.iter2 test Cases.test_cases Results.test_cases
end

module Attempt3 =
struct
  module Lang_rules =
  struct
    type type_ = Int | Bool | Unit | Node
    type rule = {
      name : string;
      parameters : (string * type_) list;
      return_type : type_;
    }

    type t = string * (rule list)

    let unit = Unit
    let int = Int
    let bool = Bool
    let t _ = Node

    let rule name parameters return_type = { name; parameters; return_type }

    let include_ _ = failwith "include_"

    let lang name rules = (name, rules)
  end
  let () = let module T : Module.Lang = Lang_rules in ()

  exception Parse_error

  module type Parse_rules =
  sig
    type t
    type reader = String_tree.t -> t
    val readers : reader -> (string * reader) list
  end

  module Calc_bool_parse_rules(L : Calc_bool.Lang) : (Parse_rules with type t = L.t) =
  struct
    type t = L.t
    type reader = String_tree.t -> t

    let readers parse =
      let binop f = function
        | String_tree.Tree [_; lhs_st; rhs_st] ->
          let lhs = parse lhs_st in
          let rhs = parse rhs_st in
          f lhs rhs
        | _ -> raise Parse_error
      in
      String_tree.[
        "bool", (function
            | Tree [_; Leaf "true"] -> L.bool true
            | Tree [_; Leaf "false"] -> L.bool false
            | _ -> raise Parse_error);
        "&&", binop L.( && );
        "||", binop L.( || );
      ]
  end

  module Calc_int_parse_rules(L : Calc_int.Lang) : (Parse_rules with type t = L.t) =
  struct
    type t = L.t
    type reader = String_tree.t -> t

    let readers parse =
      let binop f = function
          | String_tree.Tree [_; lhs_st; rhs_st] ->
            let lhs = parse lhs_st in
            let rhs = parse rhs_st in
            f lhs rhs
          | _ -> raise Parse_error
      in
      String_tree.[
      "int", (function
              | Tree [_; Leaf value] -> L.int (int_of_string value)
              | _ -> raise Parse_error);
      "+.", binop L.( +. );
      "-.", binop L.( -. );
      "*.", binop L.( *. );
      "/.", binop L.( /. );
    ]
  end

  module Parse(P : Parse_rules) =
  struct
    type t = P.t
    type reader = String_tree.t -> t

    let rec parse st =
      let readers = P.readers parse in
      let hd =
        match st with
        | String_tree.Leaf str -> str
        | String_tree.Tree (String_tree.Leaf str :: _) -> str
        | _ -> raise Parse_error
      in
      let _, reader = List.find (fun (name, _) -> name = hd) readers in
      reader st
  end

  module Int_cases(St : String_lang) =
  struct
    let test_cases = St.[
        tree [leaf "int"; leaf "0"];
        tree [leaf "int"; leaf "99"];
        tree [leaf "+.";
              tree [leaf "int"; leaf "2"];
              tree [leaf "int"; leaf "3"]];
        tree [leaf "*.";
              tree [leaf "+."; tree [leaf "int"; leaf "5"]; tree [leaf "int"; leaf "4"]];
              tree [leaf "int"; leaf "10"]]
      ]
  end

  let test_bool () =
    print_endline "Testing Experimental.Attempt3 parsing";
    let module P = Parse(Calc_bool_parse_rules(Calc_bool.To_string)) in
    let check st =
      let ast = P.parse st in
      Printf.printf "  %s => %s\n" (String_tree.to_string st) ast
    in
    let module C = Attempt2.Test_cases(String_tree) in
    List.iter check C.test_cases

  let test_int () =
    print_endline "Testing Experimental.Attempt3 parsing";
    let module P = Parse(Calc_int_parse_rules(Calc_int.To_string)) in
    let check st =
      let ast = P.parse st in
      Printf.printf "  %s => %s\n" (String_tree.to_string st) ast
    in
    let module C = Int_cases(String_tree) in
    List.iter check C.test_cases

  let test () =
    test_bool ();
    test_int ()
end

let test () =
  Attempt1.test();
  Attempt2.test();
  Attempt3.test();
  ()

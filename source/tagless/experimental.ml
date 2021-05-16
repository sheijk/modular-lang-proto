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
  ()

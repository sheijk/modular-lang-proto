
exception Parse_error of string * (Strlang.Tree.t option)
let parse_error str = raise (Parse_error (str, None))
let parse_error_at str st = raise (Parse_error (str, Some st))

let binop f = fun parse st ->
  match st with
  | Strlang.Tree.Tree [_; lhs_st; rhs_st] ->
    let lhs = parse lhs_st in
    let rhs = parse rhs_st in
    f lhs rhs
  | st -> parse_error_at "invalid binop" st

module type Rules =
sig
  type t
  type reader = Strlang.Tree.t -> t
  val readers : (string * (reader -> reader)) list
end

module Parse(P : Rules) =
struct
  type t = P.t
  type reader = Strlang.Tree.t -> t

  let rec parse st =
    let hd =
      match st with
      | Strlang.Tree.Leaf str -> str
      | Strlang.Tree.Tree (Strlang.Tree.Leaf str :: _) -> str
      | st -> parse_error_at "list as first element" st
    in
    let _, reader =
      try
        List.find (fun (name, _) -> name = hd) P.readers
      with Not_found ->
        parse_error_at ("identifier " ^ hd ^ " not found") st
    in
    reader parse st
end

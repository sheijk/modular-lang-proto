module type Lang =
sig
  type t
  val leaf : string -> t
  val tree : t list -> t
end

module Tree =
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
end
let () = let module M : Lang = Tree in ()

module To_string : (Lang with type t = string) =
struct
  type t = string

  let leaf str = str
  let tree children = "(" ^ String.concat " " children ^ ")"
end

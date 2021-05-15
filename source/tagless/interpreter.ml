
module type Empty =
sig
  type t
  type value
end

module type Values =
sig
  type value

  val int : int -> value
  val bool : bool -> value
  val unit : value
end

module type Create =
sig
  include Empty

  val make : unit -> t

  include Values
end

module type Variables =
sig
  include Empty

  val with_variable : t -> string -> t
  val get : t -> string -> int
  val set : t -> string -> int -> unit
end

module type Loop =
sig
  include Empty

  val with_index : t -> int -> t
  val loop_index : t -> int option

  include Values
end

module type All =
sig
  include Empty
  include Create with type t := t
  include Loop with type t := t
  include Variables with type t := t
  include Values
end

module Default_values =
struct
  type value = Int of int | Bool of bool | Unit

  let int i = Int i
  let bool b = Bool b
  let unit = Unit
end

module No_runtime : Create =
struct
  type t = unit
  let make () = ()

  include Default_values
end

exception Unknow_variable of string

module Dynamic : All =
struct
  type t = {
    index : int option;
    variables : (string * int ref) list;
  }

  include struct
    let make () =
      { index = None; variables = [] }
  end

  include struct
    let with_index ctx index =
      { ctx with index = Some index; }

    let loop_index ctx =
      ctx.index
  end

  include struct
    let with_variable ctx name =
      { ctx with variables = (name, ref 0) :: ctx.variables }

    let find_variable ctx name =
      let rec find = function
        | (var, r) :: _ when var = name -> r
        | _ :: remaining -> find remaining
        | [] -> raise (Unknow_variable name)
      in
      find ctx.variables

    let get ctx name =
      let r = find_variable ctx name in
      !r

    let set ctx name value =
      let r = find_variable ctx name in
      r := value
  end

  include Default_values
end

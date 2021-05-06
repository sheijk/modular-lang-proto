
module type Empty =
sig
  type t
end

module type Create =
sig
  type t

  val make : unit -> t
end

module type Variables =
sig
  type t

  val with_variable : t -> string -> t
  val get : t -> string -> int
  val set : t -> string -> int -> unit
end

module type Loop =
sig
  type t

  val with_index : t -> int -> t
  val loop_index : t -> int option
end

module type All =
sig
  type t
  include Create with type t := t
  include Loop with type t := t
  include Variables with type t := t
end

module No_runtime : Create =
struct
  type t = unit
  let make () = ()
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
end

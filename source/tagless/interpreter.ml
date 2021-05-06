
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

module Dynamic : All =
struct
  type t = Interpreter_context.t

  let make = Interpreter_context.make

  let with_variable = Interpreter_context.with_variable
  let get = Interpreter_context.get_variable
  let set = Interpreter_context.set_variable

  let with_index = Interpreter_context.with_index
  let loop_index ctx = ctx.Interpreter_context.index
end


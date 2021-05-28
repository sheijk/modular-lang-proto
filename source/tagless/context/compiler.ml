
module Location =
struct
  type t = { file : string; line : int; column : int; }
  let at file line column = { file; line; column }
end

module Ocaml_result = Result

module Result =
struct
  type error = {
    loc : Location.t option;
    message : string;
  }

  type 'a t = ('a, error list) Result.t
  let ok result = Result.ok result
  let error loc message = Result.error [{ loc; message }]
  let error_no_loc message = error None message
  let errors e = Result.error e

  let bind = Result.bind
  let apply f x = Result.map x f
  let product a b =
    match a, b with
    | Result.Ok a, Result.Ok b ->
      Result.Ok (a, b)
    | Error ae, Error be ->
      Result.Error (ae @ be)
    | Error e, Ok _
    | Ok _, Error e ->
      Result.Error e

  module Syntax =
  struct
    let ( let+ ) = apply
    let ( and+ ) = product
    let ( let* ) = bind
    let ( and* ) = product
  end
end

module Info =
struct
  type t = {
    loop_count : int;
    unbound_loop_index : bool;
    loc : Location.t option;
  }

  let make () = { loop_count = 0; unbound_loop_index = false; loc = None; }
  let at loc i = { i with loc = Some loc }

  let validate ctx = ctx.unbound_loop_index = false

  let merge a b = {
    loop_count = max a.loop_count b.loop_count;
    unbound_loop_index = a.unbound_loop_index || b.unbound_loop_index;
    loc = match a.loc with Some _ -> a.loc | None -> b.loc;
  }

  let mark_loop_index ctx = { ctx with unbound_loop_index = true; }
  let mark_loop ctx =
    let loop_num = ctx.loop_count in
    loop_num, { ctx with loop_count = loop_num + 1; unbound_loop_index = false; }
end

module Context =
struct
  module I = Interpreter.No_runtime

  type t = {
    variables : (string * I.value ref) list;
    loop_index : int ref option;
  }

  let make () = { variables = []; loop_index = None; }
  let new_loop_index ctx store = { ctx with loop_index = Some store; }
  let new_variable ctx name store =
    { ctx with variables = (name, store) :: ctx.variables; }

  let find_variable ctx name =
    let rec find = function
      | (var, r) :: _ when var = name -> Result.ok r
      | _ :: remaining -> find remaining
      | [] -> Result.error_no_loc ("accessed undefined variable " ^ name)
    in
    find ctx.variables
end

module Static_value =
struct
  type value_structure =
    | Known_int of int
    | Known_bool of bool
    | Dynamic

  let merge_value a b =
    if a = b then a
    else Dynamic

  type termination = Terminates_maybe | Terminates_always | Terminates_never

  let merge_termination a b =
    if a = b then a
    else Terminates_maybe

  type t = {
    value : value_structure;
    termination : termination;
  }

  let to_string info =
    let terminates_str =
      match info.termination with
       | Terminates_maybe -> ", might diverge"
       | Terminates_always -> ", always terminates"
       | Terminates_never -> ", never terminates"
    in
    match info.value with
    | Dynamic ->
      "dynamic" ^ terminates_str
    | Known_int i ->
      Printf.sprintf "known int %d%s" i terminates_str
    | Known_bool b ->
      Printf.sprintf "known bool %b%s" b terminates_str

  let is_known = function
    | { value = (Known_int _ | Known_bool _); _ } -> true
    | { value = Dynamic; _ } -> false

  let unknown =
    {
      value = Dynamic;
      termination = Terminates_maybe;
    }

  let with_bool info b =
    {
      info with
      value = Known_bool b;
    }

  let with_int info i =
    {
      info with
      value = Known_int i;
    }

  let bool = with_bool unknown
  let int = with_int unknown

  let terminates info =
    { info with termination = Terminates_always; }

  let merge a b =
    {
      value = merge_value a.value b.value;
      termination = merge_termination a.termination b.termination;
    }
end

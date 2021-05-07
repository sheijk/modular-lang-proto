
module Info =
struct
  type t = {
    loop_count : int;
    unbound_loop_index : bool;
  }

  let make () = { loop_count = 0; unbound_loop_index = false; }

  let validate ctx = ctx.unbound_loop_index = false

  let merge a b = {
    loop_count = max a.loop_count b.loop_count;
    unbound_loop_index = a.unbound_loop_index || b.unbound_loop_index;
  }

  let mark_loop_index ctx = { ctx with unbound_loop_index = true; }
  let mark_loop ctx =
    let loop_num = ctx.loop_count in
    loop_num, { loop_count = loop_num + 1; unbound_loop_index = false; }
end

module Context =
struct
  type t = {
    variables : (string * int ref) list;
    loop_index : int ref option;
  }

  let make () = { variables = []; loop_index = None; }
  let new_loop_index ctx store = { ctx with loop_index = Some store; }
  let new_variable ctx name store =
    { ctx with variables = (name, store) :: ctx.variables; }

  let find_variable ctx name =
    let rec find = function
      | (var, r) :: _ when var = name -> r
      | _ :: remaining -> find remaining
      | [] -> failwith ("accessed undefined variable " ^ name)
    in
    find ctx.variables
end

module Static_value =
struct
  type t = {
    known_int : int option;
    known_bool : bool option;
  }

  let to_string = function
    | { known_int = None; known_bool = None } -> "dynamic"
    | { known_int = Some i; known_bool = None } -> string_of_int i
    | { known_int = None; known_bool = Some b } -> string_of_bool b
    | { known_int = Some _; known_bool = Some _ } -> "invalid, internal error"

  let is_known = function
    | { known_bool = Some _; _ }
    | { known_int = Some _; _ } ->
      true
    | _ ->
      false

  let unknown = { known_int = None; known_bool = None; }

  let bool b = { unknown with known_bool = Some b; }
  let int i = { unknown with known_int = Some i; }
end

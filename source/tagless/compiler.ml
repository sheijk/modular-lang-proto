
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
    variables : (string * int) list;
    loop_index : int ref option;
  }

  let make () = { variables = []; loop_index = None; }
  let new_loop_index ctx store = { ctx with loop_index = Some store; }
end

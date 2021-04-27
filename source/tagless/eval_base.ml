
type context = {
  index : int option;
}

let new_context () = { index = None }

module type I = sig type 'a t = context -> 'a end
module T = struct type 'a t = context -> 'a end


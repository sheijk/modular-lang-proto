
module type I = sig type 'a t = unit -> 'a end
module T = struct type 'a t = unit -> 'a end



type t = {
  index : int option;
  variables : (string * int ref) list;
}

let make () = { index = None; variables = [] }

let with_index ctx index = { ctx with index = Some index; }

let with_variable ctx name =
  { ctx with variables = (name, ref 0) :: ctx.variables }

exception Unknow_variable of string

let find_variable ctx name =
  let rec find = function
    | (var, r) :: _ when var = name -> r
    | _ :: remaining -> find remaining
    | [] -> failwith "find_variable"
  in
  find ctx.variables

let get_variable ctx name =
  let r = find_variable ctx name in
  !r

let set_variable ctx name value =
  let r = find_variable ctx name in
  r := value

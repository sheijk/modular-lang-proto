
module type Empty =
sig
  type t
  type value
end

module type Values =
sig
  include Empty

  val value_string : value -> string

  val int : int -> value
  val bool : bool -> value
  val unit : value

  val match_bool : (bool -> 'a) -> ?error:(value -> 'a) -> value -> 'a
  val match_int : (int -> 'a) -> ?error:(value -> 'a) -> value -> 'a
  val match_unit : (unit -> 'a) -> ?error:(value -> 'a) -> value -> 'a
end

module Value_utils(V : Values) =
struct
  let match2 m1 m2 f l r =
    m1 (fun l_bool ->
        m2 (fun r_bool ->
            f l_bool r_bool
          )
          r)
      l

  let match_bool_bool f ?error l r =
    let match_bool = V.match_bool ?error in
    match2 match_bool match_bool f l r

  let match_int_int f ?error l r =
    let match_int = V.match_int ?error in
    match2 match_int match_int f l r
end

module type Create =
sig
  include Values

  val make : unit -> t
end

module type Variables =
sig
  include Empty
  type value

  val with_variable : t -> string -> t
  val get : t -> string -> value
  val set : t -> string -> value -> unit
end

module type Loop =
sig
  include Values

  val with_index : t -> int -> t
  val loop_index : t -> int option
end

module Default_values =
struct
  type value = Int of int | Bool of bool | Unit
  exception Type_error of value

  let value_string = function
    | Int i -> string_of_int i
    | Bool b -> string_of_bool b
    | Unit -> "()"

  let int i = Int i
  let bool b = Bool b
  let unit = Unit

  let raise_type_error v = raise (Type_error v)

  let match_bool f ?error v =
    match v, error with
    | Bool b, _ -> f b
    | _, Some fail -> fail v
    | _, None -> raise (Type_error v)

  let match_int f ?error v =
    match v, error with
    | Int i, _ -> f i
    | _, Some fail -> fail v
    | _, None -> raise (Type_error v)

  let match_unit f ?error v =
    match v, error with
    | Unit, _ -> f ()
    | _, Some fail -> fail v
    | _, None -> raise (Type_error v)
end
let () = let module T : Values =
         struct
           type t = unit
           include Default_values
         end in ()

module type All =
sig
  type value = Default_values.value
  include Empty with type value := value
  include Values with type value := value
  include Create with type t := t and type value := value
  include Loop with type t := t and type value := value
  include Variables with type t := t and type value := value
end

module No_runtime : sig
  include Create with type value = Default_values.value and type t = unit
end = struct
  type t = unit
  let make () = ()

  include Default_values
end

exception Unknow_variable of string

module Dynamic : All =
struct
  include Default_values

  type t = {
    index : int option;
    variables : (string * value ref) list;
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
      { ctx with variables = (name, ref (int 0)) :: ctx.variables }

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

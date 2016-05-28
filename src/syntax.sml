structure Syntax =
struct
  datatype jsonValue =
    Pair of (string * jsonValue)
  | Array of jsonValue list
  | Null
  | Float of real
  | String of string
  | Bool of bool
  | Int of int
  | Obj of jsonValue list

end

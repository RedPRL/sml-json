structure JsonSyntax =
struct
  datatype json_value =
    Pair of (string * json_value)
  | Array of json_value list
  | Null
  | Float of real
  | String of string
  | Bool of bool
  | Int of int
  | Obj of json_value list
end

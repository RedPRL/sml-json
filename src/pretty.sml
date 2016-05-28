structure Pretty =
struct
  open Syntax

  fun toString obj =
    case obj of
      Null => "null"
    | Int i => Int.toString i
    | Float f => Real.toString f
    | String s => "\"" ^ s ^ "\""
    | Bool b => Bool.toString b
    | Pair (a, b) => "\"" ^ a ^ "\": " ^ toString b
    | Array l => "[" ^ String.concatWith ", " (List.map (fn x => toString x) l)  ^ "]"
    | Obj l => "{" ^ String.concatWith ", " (List.map (fn x => toString x) l)  ^ "}"

end

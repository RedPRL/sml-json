structure JsonPretty =
struct
  open JsonSyntax

  fun toString obj =
    case obj of
      Null => "null"
    | Int i => Int.toString i
    | Float f => Real.toString f
    | String s => "\"" ^ s ^ "\""
    | Bool b => Bool.toString b
    | Array l => "[" ^ String.concatWith ", " (List.map toString l)  ^ "]"
    | Obj l => "{" ^ String.concatWith ", " (List.map prettyPair l)  ^ "}"

  and prettyPair (k, v) =
    "\"" ^ k ^ "\": " ^ toString v

end

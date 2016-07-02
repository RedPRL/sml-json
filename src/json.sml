structure Json =
struct
  open JsonSyntax
  open JsonPretty

  fun eq (q, w) =
    case (q, w) of
      (Null, Null) => true
    | (Float a, Float b) => Real.== (a, b)
    | (String a, String b) => a = b
    | (Bool a, Bool b) => a = b
    | (Int a, Int b) => a = b
    | (Array a, Array b) => ListPair.allEq eq (a, b)
    | (Pair (x, y), Pair (m, n)) => x = m andalso eq (y, n)
    | (Obj a, Obj b) => ListPair.allEq eq (a, b)
    | _ => false

  fun parse str =
    case CharParser.parseString (JsonParser.jsonObject()) str of
      Sum.INL s => raise Fail ("can't parse: " ^ s)
    | Sum.INR t => t

  fun getValueByKey obj key =
    case obj of
      Obj l => List.find (fn (Pair (k, v)) => k = key | _ => false) l
    | _ => NONE
end

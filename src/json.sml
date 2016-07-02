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
    | (Obj a, Obj b) => ListPair.allEq (fn ((x, m), (y, n)) => x = y andalso eq (m, n)) (a, b)
    | _ => false

  fun parse str =
    case CharParser.parseString (JsonParser.jsonObject()) str of
      Sum.INL s => raise Fail ("can't parse: " ^ s)
    | Sum.INR t => t

  fun getValueByKey obj key =
    case obj of
      Obj l => Option.map #2 (List.find (fn (k, v) => k = key) l)
    | _ => NONE
end

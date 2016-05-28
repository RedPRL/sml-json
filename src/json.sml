structure Json =
struct
  open Sum
  open Syntax
  open Pretty

  fun jsonEqual q w =
    case (q, w) of
      (Null, Null) => true
    | (Float a, Float b) => Real.== (a, b)
    | (String a, String b) => a = b
    | (Bool a, Bool b) => a = b
    | (Int a, Int b) => a = b
    | (Array a, Array b) => ListPair.allEq (fn (x, y) => jsonEqual x y) (a, b)
    | (Pair (x, y), Pair (m, n)) => x = m andalso (jsonEqual y n)
    | (Obj a, Obj b) => ListPair.allEq (fn (x, y) => jsonEqual x y) (a, b)
    | _ => false

  fun parse str =
    case CharParser.parseString (Parser.jsonObject()) str of
      INL s => raise Fail ("can't parse: " ^ s)
    | INR t => t

  fun getValueByKey obj key =
    case obj of
      Obj l => List.find (fn (Pair (k, v)) => k = key) l
    | _ => NONE
end

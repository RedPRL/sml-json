structure Json =
struct
  open Sum

  datatype jsonValue =
    Pair of (string * jsonValue)
  | Array of jsonValue list
  | Null
  | Float of real
  | String of string
  | Bool of bool
  | Int of int

  fun jsonEqual q w =
    case (q, w) of
      (Null, Null) => true
    | (Float a, Float b) => Real.== (a, b)
    | (String a, String b) => a = b
    | (Bool a, Bool b) => a = b
    | (Int a, Int b) => a = b
    | (Array a, Array b) => ListPair.allEq (fn (x, y) => jsonEqual x y) (a, b)
    | (Pair (x, y), Pair (m, n)) => x = m andalso (jsonEqual y n)
    | _ => false

  structure Parser =
  struct
    open ParserCombinators CharParser

    infixr 4 << >>
    infixr 3 &&
    infix  2 --
    infix  2 wth suchthat return guard when
    infixr 1 || <|> ??

    val skipChars = repeatSkip (space <|> tab <|> newLine)
    fun skipAround p = skipChars >> p << skipChars

    val anyString = repeat1 (noneOf [#"\""]) wth String.implode
    val stringInQuotes = char #"\"" >> anyString << char #"\""

    fun IntfromStringOrZero s =
      case Int.fromString s of
        NONE => 0
      | SOME d => d

    fun FloatfromStringOrZero s =
      case Real.fromString s of
        NONE => 0.0
      | SOME d => d

    val jsonNull = string "null" return Null
    val jsonTrue = string "true" return (Bool true)
    val jsonFalse = string "false" return (Bool false)
    val jsonString = stringInQuotes wth String
    val jsonInt = opt (char #"-") && repeat1 digit wth
      (fn (NONE, x) => Int (IntfromStringOrZero (String.implode x))
        | (SOME _, x) => Int (~(IntfromStringOrZero (String.implode x))))
    val jsonFloat = opt (char #"-") && repeat1 digit && char #"." >> repeat1 digit wth
      (fn (NONE, (x, y)) => Float (FloatfromStringOrZero (String.implode (x@[#"."]@y)))
        | (SOME _, (x, y)) => Float (~(FloatfromStringOrZero (String.implode (x@[#"."]@y)))))
    val jsonValue = alt [jsonNull, jsonTrue, jsonFalse, jsonInt, jsonFloat, jsonString]

    fun jsonParser() = $jsonObject || $jsonArray || jsonValue
    and jsonArray() = char #"[" >> (separate (skipAround ($jsonParser)) (char #",")) << char #"]" wth Array
    and jsonObject() = char #"{" >> (separate (skipAround ($jsonPair)) (char #",")) << char #"}" wth Array
    and jsonPair() = (skipAround stringInQuotes) && char #":" >> skipAround ($jsonParser) wth Pair

  end

  fun parse str =
    case CharParser.parseString (Parser.jsonObject()) str of
      INL s => raise Fail ("can't parse: " ^ s)
    | INR t => t

  fun getValueByKey obj key =
    case obj of
      Array l => List.find (fn (Pair (k, v)) => k = key) l
    | _ => NONE
end

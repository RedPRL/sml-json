structure Parser =
struct
  open Syntax

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
  and jsonObject() = char #"{" >> (separate (skipAround ($jsonPair)) (char #",")) << char #"}" wth Obj
  and jsonPair() = (skipAround stringInQuotes) && char #":" >> skipAround ($jsonParser) wth Pair

end

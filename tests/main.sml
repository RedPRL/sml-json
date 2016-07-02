structure Main =
struct
  structure J = Json

  fun testParse expected text =
    let
      val result = J.parse text
    in
      case J.eq (result, expected) of
        true => print "Ok\n"
      | false => raise Fail ("Fail :(")
    end

  val test1 =
    let
      val expected = J.Obj ([J.Pair ("name", J.String "Mathew"), J.Pair ("age", J.Int 20), J.Pair ("hobby", J.Array [J.Int 2, J.Int 3, J.String "lol"])])
      val text = "{\"name\":\"Mathew\",\"age\":20, \"hobby\":[2, 3, \"lol\"]}"
    in
      testParse expected text
    end
end

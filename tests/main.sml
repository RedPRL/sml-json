structure Main =
struct
    open Json

    fun testParse expected text =
      let
        val result = parse text
      in
        case jsonEqual result expected of
          true => print "Ok\n"
        | false => raise Fail ("Fail :(")
      end

    val test1 =
      let
        val expected = Obj ([Pair ("name", String "Mathew"),
                             Pair ("age", Int 20),
                             Pair ("hobby", Array [Int 2, Int 3, String "lol"])])
        val text = "{\"name\":\"Mathew\",\"age\":20, \"hobby\":[2, 3, \"lol\"]}"
      in
        testParse expected text
      end
end

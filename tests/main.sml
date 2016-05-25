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
        val expected = Array ([Pair ("name", String "Mathew"), Pair ("age", Int 20), Pair ("hobby", Array [])])
        val text = "{\"name\":\"Mathew\",\"age\":20, \"hobby\":[]}"
      in
        testParse expected text
      end
end

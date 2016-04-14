/**
  * Created by dengels on 13/04/16.
  */
object InterpolatingThemStrings {


  def main(args: Array[String]): Unit = {
    val name = "James"
    val height = 1.9f
    val id = 45

    val simple = s"Hello, my name is $name"
    println(simple)
    val simple2 = s"(1 + 1) = ${1 + 1}"
    println(simple2)

    val format = f"$name%s is $height%2.2f meters tall"
    println(format)
    // Does not compile (type mismatch): val format2 = f"$height%4d"
    // To be able to fail at compile time, the f string interpolator
    // is implemented as a def macro

    val raw = raw"This is like simple : my name is $name"
    println(raw)
    val raw2 = raw"With a slight \n difference"
    println(raw2)


    val json = json"{ name: $name, id: $id, num: ${4 * 999} }"
    println(json)
  }

  case class JSONObject(elems: Seq[(String, String)]) {
    override def toString = elems.map {
      case (key, value) => "  " + key + ": " + value
    }.mkString("{\n", ",\n", "\n}")
  }

  implicit class JsonHelper(val sc: StringContext) extends AnyVal {

    private def keyAfter(c: Char)(part: String): String =
      part.dropWhile(_ != c).tail.takeWhile(_ != ':')

    private def head(part: String): String = keyAfter('{')(part)

    private def center(part: String): String = keyAfter(',')(part)

    def json(args: Any*): JSONObject = {
      val parts = sc.parts
      val values = args.map(_.toString)
      val elems = (head(parts.head) -> values.head) +: ((parts.tail.init map center) zip values.tail)
      JSONObject(elems)
    }
  }

}

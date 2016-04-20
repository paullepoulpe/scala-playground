/**
  * From : http://docs.scala-lang.org/overviews/quasiquotes/setup
  */


import scala.reflect.runtime.universe._
import scala.reflect.runtime.currentMirror

import scala.tools.reflect.ToolBox


object QuasiquotesTutorial {

  val toolbox = currentMirror.mkToolBox()

  def println(x: Any) = Predef.println(x + "\n")

  def title(name: String) = println(name + "\n" + ("=" * name.length))

  def showAll(q: Tree): Unit = {
    Predef.println(q)
    Predef.println(showCode(q))
    Predef.println(showRaw(q))

    try {
      println("type : " + toolbox.typecheck(q).tpe)
    } catch {
      case e: scala.tools.reflect.ToolBoxError => println(e.message)
    }

  }

  def main(args: Array[String]): Unit = {
    setup()
    intro()
    lifting()
    unlifting()
    jit()
    details()
  }

  def setup(): Unit = {
    title("Dependencies and setup")
    val C = q"class C"
    println(C)
    println(showCode(C))
    println(showRaw(C))
  }

  def intro(): Unit = {
    title("Introduction")
    val tree = q"i am { a quasiquote }"
    println(tree)
    println(tree match { case q"i am { a quasiquote }" => "it worked!" })
    println(q"foo + bar" equalsStructure q"foo.+(bar)")
    val q"i am $what" = q"i am { a quasiquote }"
    println(what)

    val x =
      q"""
         val x: List[Int] = List(1, 2) match {
           case List(a, b) => List(a + b)
         }
       """
    println(showRaw(x))
    println(showRaw(q"List"))
    println(showRaw(tq"List"))

    println(showRaw(q"List(a, b)"))
    println(showRaw(pq"List(a, b)"))


    val ab = List(q"a", q"b")
    val fab = q"f(..$ab)"
    showAll(fab)

    val c = q"c"
    val fabc = q"f($c, ..$ab) + g(..$ab, $c)"
    showAll(fabc)

    val argss = List(ab, List(c))
    val fargss = q"f(...$argss)"
    showAll(fargss)

    def partialExtract: PartialFunction[Tree, Unit] = {
      case q"f($first, ..$rest)" => // ok
      case q"f(..$init, $last)" => // ok
      //case q"f(..$a, ..$b)"      => // not allowed
      case q"f(..$first)(...$rest)" => // ok
      case q"f(...$init)(..$first)" => // ok
      //case q"f(...$a)(...$b)"       => // not allowed
    }
  }

  def lifting(): Unit = {
    title("Lifting")

    showAll(q"${2} + 3")

    class Point(val x: Int, val y: Int)
    object Point {
      implicit val lift = Liftable[Point] { p =>
        q"Point(${p.x}, ${p.y})"
      }

      def apply(x: Int, y: Int) = new Point(x, y)
    }

    showAll(q"${Point(1, 2)}")

    showAll(q"${val d: Double = 4.5; d}")
    showAll(q"${val c: Char = 45.toChar; c}")
    showAll(q"${List(1, 2, 3).toString}")
    showAll(q"${List(1, 2, 3)}")

  }

  def unlifting(): Unit = {
    title("Unlifting")

    val q"${left: Int} + ${right: Int}" = q"2 + 2"
    println(left + right)

    val q"f(..${ints: List[Int]})" = q"f(1, 2, 3)"
    println(ints)

    val q"f(...${intss: List[List[Int]]})" = q"f(1, 2, 3)(4, 5)(6)"
    println(intss)

    class Point(val x: Int, val y: Int) {
      override def toString = s"MaPoint($x, $y)"
    }
    object Point {
      implicit val unliftPoint = Unliftable[Point] {
        case q"Point(${x: Int}, ${y: Int})" => Point(x, y)
      }

      def apply(x: Int, y: Int) = new Point(x, y)
    }

    val q"${p: Point}" = q"Point(1, 3)"
    println(p)

  }

  def jit(): Unit = {
    title("Just in time compilation")

    val code = q"""println("compiled and run at runtime!")"""
    val compiledCode = toolbox.compile(code)
    val result = compiledCode()
  }

  def details(): Unit = {
    title("Additional expressions")

    showAll(q"")

    showAll(q"(1, 2)")
    showAll(q"(1)")
    showAll(q"()")

    showAll(q"a; b; c")
    showAll(q"{2}")
    showAll(q"{val x = 2}")
    showAll(q"{}")

    showAll(q"if (true) 3 else 5")
    showAll(q"if (true) 3")

    showAll(q"_ + 1")
    showAll(q"(a => a + 1)")
    showAll(q"(a: Int) => a + 1")
  }
}

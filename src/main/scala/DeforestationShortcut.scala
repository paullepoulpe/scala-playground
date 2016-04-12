/*
 * Examples presented in the paper "A Short Cut to Deforestation"
 * http://dl.acm.org/citation.cfm?id=165214
 *
 * The lifting is inspired by: https://github.com/TiarkRompf/lms-playground/blob/master/src/test/scala/scala/lms/playground/test1.scala
 */

// Get all of the good stuff
import scala.language._
import scala.annotation.implicitNotFound
import scala.annotation.unchecked.uncheckedVariance


trait Common {
  @implicitNotFound("${T} is not a supported type")
  type Typ[T]

  @implicitNotFound("${A} cannot be viewed as ${B}")
  trait CanBe[A, B] {
    def apply(a: A): B
  }


  def fromFunction[A, B](f: A => B): A CanBe B = new CanBe[A, B] {
    def apply(x: A) = f(x)
  }

  implicit def identLift[T: Typ]: T CanBe T = new CanBe[T, T] {
    def apply(x: T) = x
  }

  implicit def lift[T, U](x: T)(implicit e: T CanBe U): U = e(x)
}

trait CommonEval extends Common {

  case class Typ[T](name: String)

  def typ[T: Manifest] = Typ[T](manifest[T].toString)

  implicit def nothingCanBeAnything[T]: Nothing CanBe T = fromFunction[Nothing, T]((x: Nothing) => x)

  implicit val NothingTyp = Typ("Nothing")
}

trait BetterExpr extends Common {

  def typ[T: Typ] = implicitly[Typ[T]]

  abstract class Expr[+T: Typ] {
    def selfTyp: Typ[T@uncheckedVariance] = implicitly[Typ[T]]
  }

  trait Typ[T] {
    def proxy(e: Expr[T]): T

    def extract(t: T): Expr[T]
  }


  def createTyp[T](pr: Expr[T] => T, ex: T => Expr[T], name: String = "???"): Typ[T] = new Typ[T] {
    def proxy(e: Expr[T]): T = pr(e)

    def extract(t: T): Expr[T] = ex(t)

    override def toString = name
  }

  // Important to have a bijective version of Lift so we can remap to non lifted type without a cast
  trait UniqueLift[A, B] {
    def apply(a: A): B

    def from: String

    def to: String
  }

  def uniq[A, B](implicit uniqueLift: UniqueLift[A, B]) = uniqueLift

  implicit def provedFromUniq[A, B](implicit uniqueLift: UniqueLift[A, B]) = fromFunction[A, B](uniqueLift.apply(_))

  case class Constant[+A, +B: Typ](x: A) extends Expr[B]

  // This allows you to base your proofs that a type A can be lifted to B (A CanBe B)
  def unit[A, B: Typ](x: A): B = implicitly[Typ[B]].proxy(Constant(x))

  def proveUniq[A: Manifest, B: Typ]: A UniqueLift B = new UniqueLift[A, B] {
    def apply(a: A) = unit(a)

    def from = manifest[A].runtimeClass.toString

    def to = typ[B].toString
  }

  case class Sym[T: Typ](name: String) extends Expr[T]

  private var numSymbols = 0

  def freshSym[T: Typ]: Sym[T] = {
    val name = s"x$numSymbols"
    numSymbols += 1
    Sym[T](name)
  }


}

trait BetterBooleans extends BetterExpr with AbstractBoolean {

  implicit def boolUniq: UniqueLift[scala.Boolean, Boolean] = proveUniq[scala.Boolean, Boolean]

  implicit def boolProof = provedFromUniq[scala.Boolean, Boolean](boolUniq)

  implicit def boolTyp = createTyp[Boolean](BetterBoolean(_), _.e, "Boolean")

  case class BoolAnd(l: Expr[Boolean], r: Expr[Boolean]) extends Expr[Boolean]

  case class BoolOr(l: Expr[Boolean], r: Expr[Boolean]) extends Expr[Boolean]

  case class BoolNot(b: Expr[Boolean]) extends Expr[Boolean]

  case class BetterBoolean(e: Expr[Boolean]) extends BooleanProxy {
    def ||(other: Boolean) = BetterBoolean(BoolOr(e, other.e))

    def &&(other: Boolean) = BetterBoolean(BoolAnd(e, other.e))

    def unary_! = BetterBoolean(BoolNot(e))
  }

  type Boolean = BetterBoolean


}

trait BetterInts extends BetterExpr with AbstractInt {
  self: BetterBooleans =>

  implicit def intUniq: UniqueLift[scala.Int, Int] = proveUniq[scala.Int, Int]

  implicit def intProof = provedFromUniq[scala.Int, Int](intUniq)

  implicit def intTyp = createTyp[Int](BetterInt(_), _.e, "Int")

  case class IntPlus(l: Expr[Int], r: Expr[Int]) extends Expr[Int]

  case class IntEqual(l: Expr[Int], r: Expr[Int]) extends Expr[Boolean]

  case class IntNotEqual(l: Expr[Int], r: Expr[Int]) extends Expr[Boolean]

  case class BetterInt(e: Expr[Int]) extends IntProxy {
    def +(other: Int): Int = BetterInt(IntPlus(e, other.e))

    def ==(other: Int): Boolean = BetterBoolean(IntEqual(e, other.e))

    def !=(other: Int): Boolean = BetterBoolean(IntNotEqual(e, other.e))
  }

  type Int = BetterInt
}

trait BetterIf extends BetterExpr with AbstractIf {
  self: BetterBooleans =>

  case class IfNode[T: Typ](cond: Expr[Boolean], thenBlock: Expr[T], elseBlock: Expr[T]) extends Expr[T]

  def _if_then_else[TB, EB, RES](cond: Boolean)(thenBlock: => TB)(elseBlock: => EB)
                                (implicit t2r: TB CanBe RES, e2r: EB CanBe RES, resTyp: Typ[RES]): RES = {

    val thenExpr: Expr[RES] = resTyp.extract(t2r(thenBlock))
    val elseExpr: Expr[RES] = resTyp.extract(e2r(elseBlock))
    val node: Expr[RES] = IfNode(cond.e, thenExpr, elseExpr)
    resTyp.proxy(node)
  }
}

trait BetterLists extends BetterExpr with AbstractList {


  case class UniqueListLift[A, B](typT: Typ[B], uniq: UniqueLift[A, B]) extends UniqueLift[scala.List[A], List[B]] {
    def apply(l: scala.List[A]): List[B] = {
      val scalaListT: scala.List[B] = l.map(uniq.apply(_))
      listProof(typT).apply(scalaListT)
    }

    def from = s"scala.List[${uniq.from}]"

    def to = s"List[${uniq.to}]"
  }

  implicit def listUniq[A, T](implicit typT: Typ[T], uniq: UniqueLift[A, T]) = UniqueListLift(typT, uniq)

  implicit def listProof[T: Typ] = new CanBe[scala.List[T], List[T]] {
    def apply(l: scala.List[T]) = l.foldRight[List[T]](emptyList[T]) { (head, tail) =>
      typ[List[T]].proxy(Cons(typ[T].extract(head), typ[List[T]].extract(tail)))
    }
  }

  implicit def listTyp[T: Typ]: Typ[List[T]] = createTyp(BetterList(_), _.l, s"List[${typ[T]}]")

  // Divergent implicits for Typ[Nothing] and Typ[List[Nothing]] => pass them explicitly
  def nothingTyp: Typ[Nothing] = createTyp[Nothing](_ => ???, _ => ???)

  def listNothingTyp: Typ[List[Nothing]] = createTyp(BetterList[Nothing](_)(nothingTyp), _.l)

  case class Cons[T: Typ](head: Expr[T], tail: Expr[List[T]]) extends Expr[List[T]]

  case object Nil extends Expr[List[Nothing]]()(listNothingTyp)

  case class BetterList[+T: Typ](l: Expr[List[T]]) extends ListProxy[T] {

  }

  type List[+T] = BetterList[T]

  def cons[T: Typ](head: T, tail: List[T]): List[T] =
    typ[List[T]].proxy(Cons(typ[T].extract(head), typ[List[T]].extract(tail)))

  def nil: List[Nothing] = listNothingTyp.proxy(Nil)
}

trait AbstractBoolean extends Common {

  implicit def boolProof: scala.Boolean CanBe Boolean

  implicit def boolTyp: Typ[Boolean]

  // TODO: Why do I have to make this explicit ???
  implicit def boolLift(b: scala.Boolean): Boolean = lift(b)(boolProof)

  trait BooleanProxy {
    def &&(that: Boolean): Boolean

    def ||(that: Boolean): Boolean
  }

  type Boolean <: BooleanProxy

  // Literals
  val _true: Boolean = true
  val _false: Boolean = false
}

trait EvaluationBoolean extends CommonEval with AbstractBoolean {

  //TODO: understand why This stuff causes an initialization error without lazy
  lazy val boolProof = fromFunction(EvaluationBooleanProxy)

  //TODO: this is probably refactorable in a typ[T] type parametric funciton in Common
  implicit val boolTyp: Typ[Boolean] = typ[Boolean]


  case class EvaluationBooleanProxy(b: scala.Boolean) extends BooleanProxy {
    def &&(that: Boolean): Boolean = b && that.b

    def ||(that: Boolean): Boolean = b || that.b

    override def toString = b.toString
  }

  type Boolean = EvaluationBooleanProxy
}


trait AbstractIf extends Common {
  self: AbstractBoolean =>

  // TODO: Make this ressemble more the scala version of if then else (without scala-virtualized)
  def _if_then_else[TB, EB, RES](cond: Boolean)(thenBlock: => TB)(elseBlock: => EB)
                                (implicit t2r: TB CanBe RES, e2r: EB CanBe RES, resTyp: Typ[RES]): RES

}

trait EvaluationIf extends CommonEval with AbstractIf {
  self: EvaluationBoolean =>
  def _if_then_else[TB, EB, RES](cond: Boolean)(thenBlock: => TB)(elseBlock: => EB)
                                (implicit t2r: TB CanBe RES, e2r: EB CanBe RES, resTyp: Typ[RES]): RES =
    if (cond.b) t2r(thenBlock) else e2r(elseBlock)
}

trait AbstractInt extends Common {

  self: AbstractBoolean =>

  implicit def intProof: scala.Int CanBe Int

  implicit def intTyp: Typ[Int]

  implicit def intLift(b: scala.Int): Int = lift(b)(intProof)

  trait IntProxy {
    def +(other: Int): Int

    def ==(other: Int): Boolean

    def !=(other: Int): Boolean
  }

  type Int <: IntProxy


  // Bring in a few literals
  val zero: Int = 0

  val _0 = zero

  val _1: Int = 1
  val _2: Int = 2
  val _3: Int = 3
  val _4: Int = 4
  val _5: Int = 5
  val _6: Int = 6
  val _7: Int = 7
}

trait EvaluationInt extends CommonEval with AbstractInt {
  self: EvaluationBoolean =>


  lazy val intProof = fromFunction(EvaluationIntProxy)
  implicit val intTyp: Typ[Int] = typ[Int]


  case class EvaluationIntProxy(i: scala.Int) extends IntProxy {
    def +(other: Int) = i + other.i

    def ==(other: Int): Boolean = i == other.i

    def !=(other: Int): Boolean = i != other.i

    override def toString = i.toString

  }

  type Int = EvaluationIntProxy

}

trait NumericProxies {
  self: AbstractInt =>

  implicit val intNumericProxy: Numeric[Int] = new Numeric[Int] {
    def compare(x: Int, y: Int): scala.Int = ???

    def fromInt(x: scala.Int): Int = x

    def minus(x: Int, y: Int): Int = ???

    def negate(x: Int): Int = ???

    def plus(x: Int, y: Int): Int = x + y

    def times(x: Int, y: Int): Int = ???

    def toDouble(x: Int): Double = ???

    def toFloat(x: Int): Float = ???

    def toInt(x: Int): scala.Int = ???

    def toLong(x: Int): Long = ???
  }
}

trait AbstractList extends Common {

  implicit def listProof[T: Typ]: scala.List[T] CanBe List[T]

  implicit def listTyp[T: Typ]: Typ[List[T]]

  implicit def listLift[T: Typ](l: scala.List[T]): List[T] = lift(l)(listProof)


  trait ListProxy[+T] {
  }

  type List[+T] <: ListProxy[T]

  def cons[T: Typ](head: T, tail: List[T]): List[T]

  def nil: List[Nothing]

  def list[T, U](a: U*)(implicit canBe: U CanBe T, typT: Typ[T]): List[T] = {
    if (a.isEmpty) nil else a.map(canBe.apply).foldRight[List[T]](nil)(cons)
  }

  def emptyList[T]: List[T] = nil
}

trait EvaluationList extends CommonEval with AbstractList {

  def listProof[T: Typ] = fromFunction[scala.List[T], List[T]](EvaluationListProxy.apply)

  def listTyp[T: Typ]: Typ[List[T]] = typ(Manifest.classType(EvaluationListProxy.getClass))

  case class EvaluationListProxy[+T: Typ](l: scala.List[T]) extends ListProxy[T] {

  }

  type List[+T] = EvaluationListProxy[T]

  def cons[T: Typ](head: T, tail: List[T]): List[T] = scala.::(head, tail.l)

  def nil: List[Nothing] = EvaluationListProxy(scala.Nil)(typ[Nothing])
}


trait AbstractFold extends Common {
  self: AbstractList with AbstractBoolean with AbstractIf with AbstractInt =>

  def foldr[A: Typ, B: Typ](k: (A, B) => B)(z: B)(xs: List[A]): B

  def build[A: Typ](g: ((A, List[A]) => List[A], List[A]) => List[A]): List[A] = g(cons, nil)


  def and(xs: List[Boolean]): Boolean = foldr[Boolean, Boolean](_ && _)(_true)(xs)

  def sum[A: Typ : Numeric](xs: List[A]): A = {
    val tc = implicitly[Numeric[A]]
    foldr(tc.plus)(tc.zero)(xs)
  }

  def elem[A: Typ](x: A)(xs: List[A]): Boolean = foldr[A, Boolean](_ == x || _)(_false)(xs)

  //def map[A, B](f: A => B)(xs: L[A]): L[B] = foldr[A, L[B]](f(_) :: _)(Nil)(xs)
  def map[A: Typ, B: Typ](f: A => B)(xs: List[A]): List[B] = build[B] { (cons, nil) =>
    foldr[A, List[B]]((a, b) => cons(f(a), b))(nil)(xs)
  }

  //def filter[A](f: A => Boolean)(xs: L[A]): L[A] = foldr[A, L[A]]((a, b) => if(f(a)) a :: b else b)(Nil)(xs)
  def filter[A: Typ](f: A => Boolean)(xs: List[A]): List[A] = build[A] { (cons, nil) =>
    foldr[A, List[A]]((a, b) => _if_then_else(f(a)) {
      cons(a, b)
    } {
      b
    })(nil)(xs)
  }

  //def ++[A](xs: L[A], ys: L[A]): L[A] =
  def ++[A: Typ, B >: A : Typ](xs: List[A], ys: List[B]): List[B] = build[B] { (cons, nil) =>
    foldr[A, List[B]]((a, b) => cons(a, b))(ys)(xs)
  }

  //def concat[A](xs: L[L[A]]): L[A] = foldr[L[A], L[A]](_ ::: _)(Nil)(xs)
  def concat[A: Typ](xs: List[List[A]]): List[A] = build[A] { (cons, nil) =>
    foldr[List[A], List[A]](++)(nil)(xs)
  }

  def foldl[A: Typ, B: Typ](k: (B, A) => B)(z: B)(xs: List[A]): B = {
    // B => B is not a DLS type
    //val inner = foldr[A, B => B]((a, b) => x => b(k(x, a)))(identity)(xs)
    //inner(z)
    ???
  }

  // TODO: understand how these come into play
  //def nil[A] = build[A] { (c, n) => n }
  //def cons[A](x: A, xs: List[A]) = build[A] { (c, n) => c(x, foldr(c)(n)(xs)) }

  // Additional functions not in the paper
  def length[A: Typ](xs: List[A]) = foldr[A, Int]((a, b) => b + 1)(0)(xs)

}

trait EvaluationFold extends CommonEval with AbstractFold {
  self: EvaluationList with EvaluationBoolean with EvaluationIf with EvaluationInt =>

  def foldr[A: Typ, B: Typ](k: (A, B) => B)(z: B)(xs: List[A]): B = xs.l.foldRight(z)(k)

}

trait ExprFold extends BetterExpr with AbstractFold {
  self: BetterLists with BetterBooleans with BetterIf with BetterInts with FunctionsExpr with TuplesExpr =>

  case class Foldr[A: Typ, B: Typ](k: Lambda[(A, B), B], z: Expr[B], xs: Expr[List[A]]) extends Expr[B] {
    val typA = typ[A]
    val typB = typ[B]
  }

  def foldr[A: Typ, B: Typ](k: (A, B) => B)(z: B)(xs: List[A]): B = {
    typ[B].proxy(Foldr(Lambda(k.tupled), typ[B].extract(z), typ[List[A]].extract(xs)))
  }
}

trait InfixListOps extends Common {
  self: AbstractList with AbstractFold with AbstractBoolean =>

  // Helpers used to call functions in an infix manner
  implicit class InfixL[A: Typ](xs: List[A]) {
    lazy val length = self.length(xs)

    def sum(implicit n: Numeric[A]) = self.sum(xs)

    def elem(x: A) = self.elem(x)(xs)

    def map[B: Typ](f: A => B) = self.map(f)(xs)

    def filter(f: A => Boolean) = self.filter(f)(xs)

    def ++[B >: A : Typ](other: List[B]) = self.++(xs, other)

    def all(p: A => Boolean) = map(p).and

    def foldr[B: Typ](z: B)(k: (A, B) => B): B = self.foldr(k)(z)(xs)

    def foldl[B: Typ](z: B)(k: (B, A) => B): B = self.foldl(k)(z)(xs)
  }

  implicit class InfixLBoolean(xs: List[Boolean]) extends InfixL[Boolean](xs) {
    def and = self.and(xs)
  }

  implicit class InfixLL[A: Typ](xs: List[List[A]]) extends InfixL[List[A]](xs) {
    def concat = self.concat(xs)
  }

}

object EvaluationTest extends Runner with CommonEval
  with EvaluationBoolean with EvaluationIf with EvaluationInt with NumericProxies
  with EvaluationList with EvaluationFold with InfixListOps {

  // For some reason, the compiler cannot infer these bad boys (and there have to be in direct scope ???)
  implicit val typListNothing: Typ[List[Nothing]] = listTyp[Nothing]
  implicit val listNothingCanBeListNothing: List[Nothing] CanBe List[Nothing] = identLift[List[Nothing]](listTyp[Nothing])


  def assertEquals(a: Boolean, b: Boolean): Unit = assert(a == b)

  def assertEquals(a: Int, b: Int): Unit = assert(a == b)

  def assertEquals[T](a: List[T], b: List[T]): Unit = assert(a == b)


  // A few checks to make sure the evaluation is correct
  def run(): Unit = {
    assertEquals(nil.and, true)
    assertEquals(list(true).and, true) //TODO : why do I have to use _true here ?
    assertEquals(list(false).and, false)
    assertEquals(list(true, true).and, true)
    assertEquals(list(false, true).and, false)
    assertEquals(list(true, false).and, false)
    assertEquals(list(false, false).and, false)

    assertEquals(emptyList[Int].sum, 0)
    assertEquals(sum(list(1, 2, 3)), 6)


    assertEquals(emptyList[Int].elem(0), false)
    assertEquals(list(1, 2, 3).elem(0), false)
    assertEquals(list(1, 0, 3).elem(0), true)

    assertEquals(list(1, 2, 3).map(_ + 2), list(3, 4, 5))
    assertEquals(emptyList[Int].map(_ + 2), nil)


    assertEquals(list(1, 2, 3).filter(_ == _2), list(2)) // untyped equals makes my code ugly
    assertEquals(list(1, 2, 3).filter(x => false), nil)
    assertEquals(list(1, 2, 3).filter(x => true), list(1, 2, 3))

    assertEquals(list(1, 2, 3) ++ list(4, 5, 6), list(1, 2, 3, 4, 5, 6))
    // need the emptyList construct because the compile would rather upcast than apply an implicit conversion
    assertEquals(emptyList[Int] ++ list(4, 5, 6), list(4, 5, 6))
    assertEquals(list(1, 2, 3) ++ nil, list(1, 2, 3))

    assertEquals(list(nil, nil, nil).concat, nil)
    assertEquals(list(nil, list(4, 5, 6)).concat, list(4, 5, 6))
    assertEquals(list(list(1, 2, 3), nil).concat, list(1, 2, 3))
    assertEquals(list(list(1, 2, 3), list(4, 5, 6)).concat, list(1, 2, 3, 4, 5, 6))

    val right = list(1, 2, 3, 4).foldr(emptyList[Int])((a, b) => cons(b.length + a, b))
    assertEquals(right, list(4, 4, 4, 4))

    //val left = list(1, 2, 3, 4).foldl(emptyList[Int])((b, a) => cons(b.length + a, b))
    //assertEquals(left, list(7, 5, 3, 1))

    println("=================== All evaluation test passed")
  }
}

trait Tuples extends Common {

}

trait TuplesExpr extends Tuples with BetterExpr {
  implicit def tupleTyp[A: Typ, B: Typ]: Typ[(A, B)] = createTyp[(A, B)]({
    case TupleExpr2(t) => t
    case t: Sym[_] => (typ[A].proxy(Select1(t)), typ[B].proxy(Select2(t)))
  }, {
    TupleExpr2(_)
  }, "(" + typ[A] + "," + typ[B] + ")")


  case class Select1[A: Typ, B: Typ](tup: Expr[(A, B)]) extends Expr[A] {
    val typA = typ[A]
    val typB = typ[B]
  }

  case class Select2[A: Typ, B: Typ](tup: Expr[(A, B)]) extends Expr[B] {
    val typA = typ[A]
    val typB = typ[B]
  }

  case class TupleExpr2[A: Typ, B: Typ](tup: (A, B)) extends Expr[(A, B)] {
    val typA = typ[A]
    val typB = typ[B]

    def _1 = Select1(this)

    def _2 = Select2(this)
  }

}

trait Functions extends Common {

}

trait FunctionsExpr extends Functions with BetterExpr {

  implicit def lambdaTyp[A: Typ, B: Typ]: Typ[A => B] = createTyp[A => B]({
    case Lambda(f) => f
  }, {
    f => Lambda(f)
  }, s"(${typ[A]}) => ${typ[B]}")


  case class UniqueFunLift[A, B, C, D](typA: Typ[A], typB: Typ[B],
                                       uniqCA: UniqueLift[C, A], uniqDB: UniqueLift[D, B]) extends UniqueLift[C => D, A => B] {

    def apply(a: C => D) = unit[C => D, A => B](a)(lambdaTyp(typA, typB))

    def from = uniqCA.from + "=>" + uniqDB.from

    def to = uniqCA.to + "=>" + uniqDB.to
  }

  implicit def uniqFunlift[A, B, C, D]
  (implicit typA: Typ[A], typB: Typ[B],
   uniqCA: UniqueLift[C, A], uniqDB: UniqueLift[D, B]): UniqueLift[C => D, A => B] = UniqueFunLift(typA, typB, uniqCA, uniqDB)

  case class Lambda[-A: Typ, +B: Typ](f: A => B) extends Expr[A => B] {
    val typA: Typ[A@uncheckedVariance] = typ[A]
    val typB: Typ[B@uncheckedVariance] = typ[B]
    lazy val symbol: Expr[A@uncheckedVariance] = freshSym[A]
    lazy val block: Expr[B] = typ[B].extract(f(typ[A].proxy(symbol)))

    def apply(a: A) = typB.proxy(Apply(this, typA.extract(a)))
  }

  case class Apply[A: Typ, B: Typ](f: Expr[A => B], arg: Expr[A]) extends Expr[B] {
    val typA = typ[A]
    val typB = typ[B]
  }

}

object ExprTest extends Runner with BetterExpr
  with BetterBooleans with BetterIf with BetterInts with NumericProxies
  with BetterLists with FunctionsExpr with TuplesExpr with ExprFold with InfixListOps {

  def string[T: Typ](t: T): String = mkString(typ[T].extract(t))

  def mkString(expr: Expr[Any]): String = expr match {
    case BoolAnd(l, r) => "(" + mkString(l) + " && " + mkString(r) + ")"
    case BoolOr(l, r) => "(" + mkString(l) + " || " + mkString(r) + ")"
    case BoolNot(b) => "!(" + mkString(b) + ")"
    case IntPlus(l, r) => "(" + mkString(l) + " + " + mkString(r) + ")"
    case IntEqual(l, r) => "(" + mkString(l) + " == " + mkString(r) + ")"
    case IntNotEqual(l, r) => "(" + mkString(l) + " != " + mkString(r) + ")"
    case IfNode(b, t, e) => "if(" + mkString(b) + "){ " + mkString(t) + " } else { " + mkString(e) + " }"
    case Cons(head, tail) => mkString(head) + " :: " + mkString(tail)
    case Nil => "Nil"
    case Constant(x) => x.toString
    case Sym(name) => name
    case s@Lambda(f) => "(" + mkString(s.symbol) + ": " + s.typA + ") => " + mkString(s.block)
    case Apply(f, arg) => "(" + mkString(f) + ").apply(" + mkString(arg) + ")"
    case Foldr(k, z, xs) => "foldr(" + mkString(k) + ")(" + mkString(z) + ")(" + mkString(xs) + ")"
    case Select1(x) => mkString(x) + "._1"
    case Select2(x) => mkString(x) + "._2"
    case x => sys.error(s"Don't know how to print $x")
  }

  def eval[T, A](expr: Expr[T])(implicit typT: Typ[T], uniq: UniqueLift[A, T]): A = {
    (expr match {
      case c@Constant(x) if c.selfTyp.toString == typT.toString => x
      case BoolAnd(l, r) => eval(l) && eval(r)
      case BoolOr(l, r) => eval(l) || eval(r)
      case BoolNot(b) => !eval(b)
      case IntPlus(l, r) => eval(l) + eval(r)
      case IntEqual(l, r) => eval(l) == eval(r)
      case IntNotEqual(l, r) => eval(l) != eval(r)
      case IfNode(cond, thn, els) => if (eval(cond)) eval(thn) else eval(els)
      case c: Cons[_] => uniq match {
        case u: UniqueListLift[a, t] =>
          val tail = eval(c.tail.asInstanceOf[Expr[List[t]]])(c.selfTyp.asInstanceOf[Typ[List[t]]], u)
          val head = eval(c.head.asInstanceOf[Expr[t]])(u.typT, u.uniq)
          head :: tail
        case _ => sys.error("Bad unique lift for list")
      }
      case Nil => scala.Nil
      case TupleExpr2(tup) => ???
      case Select1(t: TupleExpr2[ta, tb]) => {
        val resExpr = t.typA.extract(t.tup._1)
        eval(resExpr)(t.typA, uniq.asInstanceOf[UniqueLift[A, ta]])
      }
      case Select2(t: TupleExpr2[ta, tb]) => {
        val resExpr = t.typB.extract(t.tup._2)
        eval(resExpr)(t.typB, uniq.asInstanceOf[UniqueLift[A, tb]])
      }
      case f: Foldr[a, b] => f.xs match {
        case Cons(head, tail) => {
          val right = eval(f.copy(xs = tail)(f.typA, f.typB))(f.typB, uniq.asInstanceOf[UniqueLift[A, b]])
          val resExpr = f.typB.extract(f.k((f.typA.proxy(head), uniq.apply(right).asInstanceOf[b])))
          eval(resExpr)(f.typB, uniq.asInstanceOf[UniqueLift[A, b]])
        }
        case Nil => eval(f.z)(f.typB, uniq.asInstanceOf[UniqueLift[A, b]])
      }
      case Lambda(f) => uniq match {
        case u: UniqueFunLift[a, b, c, d] =>
          (x: c) => {
            val param: a = u.uniqCA(x)
            val res: b = f.asInstanceOf[a => b](param)
            eval[b, d](u.typB.extract(res))(u.typB, u.uniqDB)
          }
        case _ => sys.error("Bad unique lift for lambda")
      } // Need a way to pattern match on type of uniq
      case a@Apply(func: Lambda[a, b], arg) => {
        val resExpr = func.typB.extract(func.f(func.typA.proxy(arg)))
        eval(resExpr)(func.typB, uniq.asInstanceOf[UniqueLift[A, b]])
      }
      case _ => sys.error(s"Don't know how to reduce $expr of type $typT")
    }).asInstanceOf[A]
  }

  def evalTyp[A, B](expr: B)(implicit typB: Typ[B], uniqueLift: UniqueLift[A, B]): A = eval(typ[B].extract(expr))


  type Subst = PartialFunction[Expr[Any], Expr[Any]]

  def transform[T: Typ](subst: Subst)(expr: Expr[T]): Expr[T] = {
    subst.lift(expr).getOrElse {
      expr match {
        case IfNode(c, t, e) => IfNode(transform(subst)(c), transform(subst)(t), transform(subst)(e))
        case IntPlus(a, b) => IntPlus(transform(subst)(a), transform(subst)(b))
        case BoolOr(a, b) => BoolOr(transform(subst)(a), transform(subst)(b))
        case BoolAnd(a, b) => BoolAnd(transform(subst)(a), transform(subst)(b))
        case s: Select1[a, b] => {
          implicit val typA = s.typA
          implicit val typB = s.typB
          Select1[a, b](transform(subst)(s.tup))
        }
        case s: Select2[a, b] => {
          implicit val typA = s.typA
          implicit val typB = s.typB
          Select2[a, b](transform(subst)(s.tup))
        }
        case t: Constant[_, _] => t
        case _ => sys.error(s"Dont know how to transform : $expr")
      }
    }.asInstanceOf[Expr[T]] // TODO : can we do without cast ?
  }


  def show[A, B](expr: B)(implicit typB: Typ[B], uniqueLift: UniqueLift[A, B]): Unit = {
    println(s"Evaluating: ${string(expr)}")
    println(s"Lifted type: ${uniqueLift.to}, evaluated type: ${uniqueLift.from}")
    println(s"Result = ${evalTyp(expr)}")
    println()
  }


  def run(): Unit = {
    show(_1 + 2 + 3)
    show(!(_true && false) || true)
    show(list(1, 2, 3, 4, 5))
    show(_if_then_else(true)(32)(14))
    show((x: Int) => x + 42)
    show(Lambda((x: Int) => x + 3).apply(45))
    show(list(1, 2, 3, 4).foldr[Int](0)(_ + _))
    show(list(1, 2, 3, 4).map(_ + 2))
    show(list(1, 2, 3, 4).filter(_ == _1))
    show(list(1, 2, 3, 4).filter(_ != _4))
  }
}

trait Runner {
  def run(): Unit
}

object Main {
  def main(args: Array[String]): Unit = {
    val runners = List(EvaluationTest, ExprTest)
    runners foreach (_.run())
  }
}

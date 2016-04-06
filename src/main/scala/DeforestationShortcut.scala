/*
 * Examples presented in the paper "A Short Cut to Deforestation"
 * http://dl.acm.org/citation.cfm?id=165214
 *
 * The lifting is inspired by: https://github.com/TiarkRompf/lms-playground/blob/master/src/test/scala/scala/lms/playground/test1.scala
 */

// Get all of the good stuff
import scala.language._


/* Simple lists with no methods */
sealed trait MyList[+T] {
  def toList: List[T]
}

object MyList {
  def apply[T](xs: T*): MyList[T] =
    if (xs.isEmpty) MyNil
    else MyCons(xs.head, MyList(xs.tail: _*))
}

case object MyNil extends MyList[Nothing] {
  override def toString = "()"

  def toList = Nil
}


case class MyCons[T](x: T, xs: MyList[T]) extends MyList[T] {
  override def toString = toList.mkString("(", ",", ")")

  def toList = x :: xs.toList
}

trait Common {
  type Typ[T]

  trait CanBe[A, B] {
    def apply(a: A): B
  }

  // Can't write this cause the compiler gets confused with implicit resolution
  //type CanBe[A, B] = A => B

  implicit def identLift[T: Typ]: T CanBe T = new CanBe[T, T] {
    def apply(x: T) = x
  }

  implicit def subtypeLift[A, B >: A]: A CanBe B = new CanBe[A, B] {
    def apply(x: A) = x
  }

  def fromFunction[A, B](f: A => B) = new CanBe[A, B] {
    def apply(x: A) = f(x)
  }

  implicit def lift[T, U](x: T)(implicit e: T CanBe U): U = e(x)
}

trait CommonEval extends Common {
  type Typ[T] = Manifest[T]
}

/* Common expression trait shared by all traits that create Expressions */
trait CommonExpr extends Common {

  abstract class ProxyTyp[T] {
    type ScalaT
    implicit def proof : ScalaT CanBe T
    def proxy(e: Expr[ScalaT]): T
    def extract(x: T): Expr[ScalaT]
  }

  def getProxy[S, T](pr: Expr[S] => T, ex: T => Expr[S])(implicit mTE: S CanBe T) = new ProxyTyp[T] {
    type ScalaT = S
    def proof = mTE
    def proxy(e: Expr[ScalaT]): T = pr(e)
    def extract(x: T): Expr[ScalaT] = ex(x)
  }

  // TODO: is this really what we want ?
  type Typ[T] = ProxyTyp[T]

  // expressions with type T
  abstract class Expr[+T]

  // extends Dynamic ?

  // Constants
  case class Constant[+T](x: T) extends Expr[T]

  // Identifiers
  case class Id[I](s: Symbol)

  // Lambda
  case class Lambda[I, T](x: Id[I], expr: Expr[T])

  //implicit def liftConstants[T](x: T): Expr[T] = Constant(x)

  implicit def liftSymbols[T](s: Symbol): Id[T] = Id[T](s)

  def string(e: Expr[_]): String = e match {
    case Constant(x) => "_" + x.toString
    case _ =>
      sys.error("Don't know how to call toString on : " + e)
  }

}

trait AbstractBoolean extends Common {

  implicit val boolProof: scala.Boolean CanBe Boolean

  // TODO: Why do I have to make this explicit ???
  implicit def boolLift(b: scala.Boolean): Boolean = lift(b)(boolProof)

  trait BooleanProxy {
    def &&(that: Boolean): Boolean

    def ||(that: Boolean): Boolean
  }

  type Boolean <: BooleanProxy

  // Literals
  val _true: Boolean = true
  val _false: Boolean = true
}

trait EvaluationBoolean extends CommonEval with AbstractBoolean {

  val boolProof = fromFunction(EvaluationBooleanProxy)

  case class EvaluationBooleanProxy(b: scala.Boolean) extends BooleanProxy {
    def &&(that: Boolean): Boolean = b && that.b

    def ||(that: Boolean): Boolean = b && that.b
  }

  type Boolean = EvaluationBooleanProxy
}

trait ExprBoolean extends AbstractBoolean with CommonExpr {

  implicit val boolProof = fromFunction((x: scala.Boolean) => ExprBoolProxy(Constant(x)))
  implicit val boolTyp: Typ[Boolean] = getProxy[scala.Boolean, Boolean](ExprBoolProxy, _.e)


  case class BoolAnd(a: Expr[scala.Boolean], b: Expr[scala.Boolean]) extends Expr[scala.Boolean]

  case class BoolOr(a: Expr[scala.Boolean], b: Expr[scala.Boolean]) extends Expr[scala.Boolean]

  case class ExprBoolProxy(e: Expr[scala.Boolean]) extends BooleanProxy {
    def &&(other: Boolean) = ExprBoolProxy(BoolAnd(e, other.e))

    def ||(other: Boolean) = ExprBoolProxy(BoolOr(e, other.e))
  }

  type Boolean = ExprBoolProxy


  override def string(e: Expr[_]): String = e match {
    case BoolAnd(x, y) => string(x) + " && " + string(y)
    case BoolOr(x, y) => string(x) + " || " + string(y)
    case _ => super.string(e)
  }
}

trait AbstractIf extends Common with AbstractBoolean {

  // TODO: Make this ressemble more the scala version of if then else (without scala-virtualized)
  def _if_then_else[TB, EB, RES: Typ](cond: Boolean)(thenBlock: => TB)(elseBlock: => EB)
                                     (implicit t2r: TB CanBe RES, e2r: EB CanBe RES): RES

}

trait EvaluationIf extends CommonEval with AbstractIf with EvaluationBoolean {
  def _if_then_else[TB, EB, RES: Typ](cond: Boolean)(thenBlock: => TB)(elseBlock: => EB)
                                     (implicit t2r: TB CanBe RES, e2r: EB CanBe RES): RES =
    if (cond.b) t2r(thenBlock) else e2r(elseBlock)
}

trait ExprIf extends AbstractIf with ExprBoolean with CommonExpr {

  case class IfNode[T](cond: Boolean, thenBlock: Expr[T], elseBlock: Expr[T]) extends Expr[T]

  def _if_then_else[TB, EB, RES: Typ](cond: Boolean)(thenBlock: => TB)(elseBlock: => EB)
                                     (implicit t2r: TB CanBe RES, e2r: EB CanBe RES): RES = {

    val proxyTyp : ProxyTyp[RES] = implicitly[Typ[RES]]
    val thenExpr : Expr[proxyTyp.ScalaT] = proxyTyp.extract(t2r(thenBlock))
    val elseExpr : Expr[proxyTyp.ScalaT]= proxyTyp.extract(e2r(elseBlock))
    val node : Expr[proxyTyp.ScalaT] = IfNode(cond, thenExpr, elseExpr)
    proxyTyp.proxy(node)
  }
}

trait AbstractInt extends Common {


  implicit val intProof: scala.Int CanBe Int

  implicit def intLift(b: scala.Int): Int = lift(b)(intProof)

  trait IntProxy {
    def +(other: Int): Int
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


  val intProof = fromFunction(EvaluationIntProxy)

  case class EvaluationIntProxy(i: scala.Int) extends IntProxy {
    def +(other: Int) = i + other.i

    def ==(other: Int) = lift(i == other.i)
  }

  type Int = EvaluationIntProxy

  implicit def lift(b: scala.Int): Int = new EvaluationIntProxy(b)
}

trait ExprInt extends AbstractInt with CommonExpr {
  implicit val intProof = fromFunction((x: scala.Int) => ExprIntProxy(Constant(x)))
  implicit val intTyp: Typ[Int] = getProxy[scala.Int, Int](ExprIntProxy, _.e)

  case class IntPlus(a: Expr[scala.Int], b: Expr[scala.Int]) extends Expr[scala.Int]

  case class ExprIntProxy(e: Expr[scala.Int]) extends IntProxy {
    def +(other: Int) = ExprIntProxy(IntPlus(e, other.e))
  }

  type Int = ExprIntProxy

  override def string(e: Expr[_]): String = e match {
    case IntPlus(x, y) => string(x) + " + " + string(y)
    case _ => super.string(e)
  }
}

trait NumericProxies extends AbstractInt {
  outer =>

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

trait AbstractWorld extends AbstractBoolean with AbstractIf with AbstractInt with NumericProxies {
  outer =>

  type L[T] // type B in the paper TODO: see if this is actually needed (too restrictive ?)

  implicit def LTpe[T]: Typ[L[T]]

  def foldr[A, B](k: (A, B) => B)(z: B)(xs: L[A]): B

  //def build[A, B](g: (B, (A, B) => B) => B): B
  def build[A](g: ((A, L[A]) => L[A], L[A]) => L[A]): L[A]


  def and(xs: L[Boolean]): Boolean = foldr[Boolean, Boolean](_ && _)(_true)(xs)

  def sum[A: Numeric](xs: L[A]): A = {
    val tc = implicitly[Numeric[A]]
    foldr(tc.plus)(tc.zero)(xs)
  }

  def elem[A](x: A)(xs: L[A]): Boolean = foldr[A, Boolean] {
    (a, b) => lift(a == x) || b
  }(_false)(xs)

  //def map[A, B](f: A => B)(xs: L[A]): L[B] = foldr[A, L[B]](f(_) :: _)(Nil)(xs)
  def map[A, B](f: A => B)(xs: L[A]): L[B] = build[B] { (cons, nil) =>
    foldr[A, L[B]]((a, b) => cons(f(a), b))(nil)(xs)
  }

  //def filter[A](f: A => Boolean)(xs: L[A]): L[A] = foldr[A, L[A]]((a, b) => if(f(a)) a :: b else b)(Nil)(xs)
  def filter[A](f: A => Boolean)(xs: L[A]): L[A] = build[A] { (cons, nil) =>
    foldr[A, L[A]]((a, b) => _if_then_else[L[A], L[A], L[A]](f(a)) {
      // TODO: having to specify the types is annoying here
      cons(a, b)
    } {
      b
    })(nil)(xs)
  }

  //def ++[A](xs: L[A], ys: L[A]): L[A] = 
  def ++[A](xs: L[A], ys: L[A]): L[A] = build[A] { (cons, nil) =>
    foldr[A, L[A]]((a, b) => cons(a, b))(ys)(xs)
  }

  //def concat[A](xs: L[L[A]]): L[A] = foldr[L[A], L[A]](_ ::: _)(Nil)(xs)
  def concat[A](xs: L[L[A]]): L[A] = build[A] { (cons, nil) =>
    foldr[L[A], L[A]](++)(nil)(xs)
  }

  def foldl[A, B](k: (B, A) => B)(z: B)(xs: L[A]): B =
    foldr[A, B => B]((a, b) => x => b(k(x, a)))(identity)(xs)(z)

  // TODO: implement these
  //def repeat x
  //def zip xs ys


  // TODO: understand how these come into play
  def nil[A] = build[A] { (c, n) => n }

  def cons[A](x: A, xs: L[A]) = build[A] { (c, n) => c(x, foldr(c)(n)(xs)) }

  // Additional functions not in the paper
  def length[A](xs: L[A]) = foldr[A, Int]((a, b) => b + 1)(0)(xs)


  // Helpers used to call functions in an infix manner
  implicit class InfixL[A](xs: L[A]) {
    lazy val length = outer.length(xs)

    def sum(implicit n: Numeric[A]) = outer.sum(xs)

    def elem(x: A) = outer.elem(x)(xs)

    def map[B](f: A => B) = outer.map(f)(xs)

    def filter(f: A => Boolean) = outer.filter(f)(xs)

    def ++(other: L[A]) = outer.++(xs, other)

    def all(p: A => Boolean) = map(p).and
  }

  implicit class InfixLBoolean(xs: L[Boolean]) extends InfixL[Boolean](xs) {
    def and = outer.and(xs)
  }

  implicit class InfixLL[A](xs: L[L[A]]) extends InfixL[L[A]](xs) {
    def concat = outer.concat(xs)
  }

}

object EvaluationWorld extends CommonEval with AbstractWorld with EvaluationBoolean with EvaluationIf with EvaluationInt {

  type L[T] = MyList[T]

  implicit def LTpe[T]: Typ[L[T]] = implicitly[Manifest[MyList[T]]]

  def foldr[A, B](k: (A, B) => B)(z: B)(xs: L[A]): B = xs match {
    case MyNil => z
    case MyCons(x, xs) => k.curried(x)(foldr(k)(z)(xs))
  }

  def build[A](g: ((A, L[A]) => L[A], L[A]) => L[A]): L[A] = g(MyCons(_, _), MyNil)


  // A few checks to make sure the evaluation is correct
  assert(and(MyNil) == _true)
  assert(and(MyList(_true)) == _true)
  assert(and(MyList(_false)) == _false)
  assert(and(MyList(_true, _true)) == _true)
  assert(and(MyList(_false, _true)) == _false)
  assert(and(MyList(_true, _false)) == _false)
  assert(and(MyList(_false, _false)) == _false)

  assert(sum[Int](MyNil) == _0)
  assert(sum(MyList(_1, _2, _3)) == _6)

  assert(elem(_0)(MyNil) == _false)
  assert(elem(_0)(MyList(_1, _2, _3)) == _false)
  assert(elem(_0)(MyList(_1, _0, _3)) == _true)

  assert(map[Int, Int](_ + _2)(MyList(_1, _2, _3)) == MyList(_3, _4, _5))
  assert(map[Int, Int](_ + _2)(MyNil) == MyNil)

  assert(filter[Int](_ == _2)(MyList(_1, _2, _3)) == MyList(_2))
  assert(filter[Int](x => false)(MyList(_1, _2, _3)) == MyNil)
  assert(filter[Int](x => true)(MyList(_1, _2, _3)) == MyList(_1, _2, _3))

  assert(++(MyList(_1, _2, _3), MyList(_4, _5, _6)) == MyList(_1, _2, _3, _4, _5, _6))
  assert(++(MyNil, MyList(_4, _5, _6)) == MyList(_4, _5, _6))
  assert(++(MyList(_1, _2, _3), MyNil) == MyList(_1, _2, _3))

  assert(concat(MyList(MyNil, MyNil, MyNil)) == MyNil)
  assert(concat(MyList(MyNil, MyList(_1, _2, _3))) == MyList(_1, _2, _3))
  assert(concat(MyList(MyList(_1, _2, _3), MyNil)) == MyList(_1, _2, _3))
  assert(concat(MyList(MyList(_1, _2, _3), MyList(_4, _5, _6))) == MyList(_1, _2, _3, _4, _5, _6))

  val right = foldr[Int, L[Int]]((a, b) => MyCons(b.length + a, b))(MyNil)(MyList(_1, _2, _3, _4))
  val left = foldl[Int, L[Int]]((b, a) => MyCons(b.length + a, b))(MyNil)(MyList(_1, _2, _3, _4))
  assert(right == MyList(_4, _4, _4, _4))
  assert(left == MyList(_7, _5, _3, _1))

  println("All evaluation test passed")

}

object ExprWorld extends AbstractWorld with CommonExpr with ExprBoolean with ExprIf with ExprInt {

  type L[T] = MyList[T]

  implicit def LTpe[T]: Typ[L[T]] = ???

  case class FoldR[A, B: Manifest](k: (A, B) => B, z: B, xs: L[A]) extends Expr[B]

  case class Build[A: Manifest](g: ((A, L[A]) => L[A], L[A]) => L[A]) extends Expr[L[A]]

  def foldr[A, B: Manifest](k: (A, B) => B)(z: B)(xs: L[A]): B = eval(FoldR(k, z, xs))

  def build[A: Manifest](g: ((A, L[A]) => L[A], L[A]) => L[A]): L[A] = eval(Build(g))

  def foldr[A, B](k: (A, B) => B)(z: B)(xs: L[A]): B = ???

  def build[A](g: ((A, L[A]) => L[A], L[A]) => L[A]): L[A] = ???

  def eval[T: Manifest](expr: Expr[T]): T = {
    val res: T = expr match {
      case FoldR(k, z, xs) => ??? //xs.toList.foldRight(z)(k)
      case Build(g) => g((a, b) => MyCons(a, b), MyNil)
      case IntPlus(a, b) => eval(a) + eval(b)
      case Constant(e) => e
      case _ =>
        println(s"Don't know how to evaluate : $expr")
        ???
    }
    val manifest = implicitly[Manifest[T]]
    println("eval[" + manifest.toString + "](" + string(expr) + ") = " + res.toString)
    res
  }

}

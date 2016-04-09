/*
 * Examples presented in the paper "A Short Cut to Deforestation"
 * http://dl.acm.org/citation.cfm?id=165214
 *
 * The lifting is inspired by: https://github.com/TiarkRompf/lms-playground/blob/master/src/test/scala/scala/lms/playground/test1.scala
 */

// Get all of the good stuff
import scala.language._
import scala.annotation.implicitNotFound

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
  override def toString = "L()"

  def toList = Nil
}


case class MyCons[T](x: T, xs: MyList[T]) extends MyList[T] {
  override def toString = toList.mkString("L(", ",", ")")

  def toList = x :: xs.toList
}

trait Common {

  @implicitNotFound("${T} is not a DSL type")
  type Typ[T]

  @implicitNotFound("${A} cannot be viewed as ${B}")
  trait CanBe[A, B] {
    def apply(a: A): B
  }

  // Can't write this cause the compiler gets confused with implicit resolution
  //type CanBe[A, B] = A => B

  def fromFunction[A, B](f: A => B) = new CanBe[A, B] {
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

/* Common expression trait shared by all traits that create Expressions */
trait CommonExpr extends Common {

  private var numSymbols = 0

  def freshSymbol(): Symbol = {
    val curSym = s"x$numSymbols"
    numSymbols += 1
    Symbol(curSym)
  }

  abstract class ProxyTyp[T] {
    type ScalaT

    def stringRep: String

    def proof: ScalaT CanBe T

    def proxy(e: Expr[ScalaT]): T

    def extract(x: T): Expr[ScalaT]

    override def toString = stringRep
  }

  def getProxy[S, T](name: String, pr: Expr[S] => T, ex: T => Expr[S])(implicit mTE: S CanBe T) = new ProxyTyp[T] {
    type ScalaT = S

    def proof = mTE

    def proxy(e: Expr[ScalaT]): T = pr(e)

    def stringRep = name

    def extract(x: T): Expr[ScalaT] = ex(x)
  }

  type Typ[T] = ProxyTyp[T]

  implicit val nothingCanBeNothing: Nothing CanBe Nothing = fromFunction[Nothing, Nothing]((x: Nothing) => x)
  implicit val NothingTyp: Typ[Nothing] = getProxy[Nothing, Nothing]("Nothing", (x: Expr[Nothing]) => ???, (x: Nothing) => Constant(???))

  // expressions with type T
  abstract class Expr[+T] {
    def toString: String

    def eval(): T
  }

  // extends Dynamic ?

  // Constants
  case class Constant[+T](x: T) extends Expr[T] {
    override def toString = x.toString

    def eval() = x
  }

  // Identifiers
  case class Id[I](s: Symbol) extends Expr[I] {
    override def toString = s.name

    def eval() = sys.error(s"Cannot evaluate free var $s")
  }


  type Subst = Map[Expr[Any], Expr[Any]]

  def transform[T](subst: Subst)(expr: Expr[T]): Expr[T] = {
    sys.error("Don't know how to transform")
  }

  // Lambda
  case class Lambda[I, T](x: Id[I], expr: Expr[T]) extends Expr[I => T] {

    override def toString = s"($x: ???) => $expr"

    def eval(): I => T = {
      (i: I) =>
        transform(Map(x -> Constant(i)))(expr).eval()
    }
  }

  implicit def liftFunction[A, B](f: A => B)(implicit typA: Typ[A], typB: Typ[B]): Lambda[typA.ScalaT, typB.ScalaT] = {
    val s = Id[typA.ScalaT](freshSymbol())
    val block = typB.extract(f(typA.proxy(s)))
    Lambda(s, block)
  }

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

trait ExprBoolean extends CommonExpr with AbstractBoolean {

  implicit lazy val boolProof = fromFunction((x: scala.Boolean) => ExprBoolProxy(Constant(x)))
  implicit val boolTyp: Typ[Boolean] = getProxy[scala.Boolean, Boolean]("Boolean", ExprBoolProxy, _.e)


  case class BoolAnd(a: Expr[scala.Boolean], b: Expr[scala.Boolean]) extends Expr[scala.Boolean] {
    override def toString = "(" + a.toString + " && " + b.toString + ")"

    def eval() = a.eval && b.eval()
  }

  case class BoolOr(a: Expr[scala.Boolean], b: Expr[scala.Boolean]) extends Expr[scala.Boolean] {
    override def toString = "(" + a.toString + " || " + b.toString + ")"

    def eval() = a.eval || b.eval()

  }

  case class ExprBoolProxy(e: Expr[scala.Boolean]) extends BooleanProxy {
    def &&(other: Boolean) = ExprBoolProxy(BoolAnd(e, other.e))

    def ||(other: Boolean) = ExprBoolProxy(BoolOr(e, other.e))
  }

  type Boolean = ExprBoolProxy
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

trait ExprIf extends CommonExpr with AbstractIf {
  self: ExprBoolean =>

  case class IfNode[T](cond: Expr[scala.Boolean], thenBlock: Expr[T], elseBlock: Expr[T]) extends Expr[T] {
    override def toString =
      s"""if($cond){
          |  $thenBlock
          |} else {
          |  $elseBlock
          |}""".stripMargin

    def eval() = if (cond.eval()) thenBlock.eval() else elseBlock.eval()
  }

  def _if_then_else[TB, EB, RES](cond: Boolean)(thenBlock: => TB)(elseBlock: => EB)
                                (implicit t2r: TB CanBe RES, e2r: EB CanBe RES, resTyp: Typ[RES]): RES = {

    val thenExpr: Expr[resTyp.ScalaT] = resTyp.extract(t2r(thenBlock))
    val elseExpr: Expr[resTyp.ScalaT] = resTyp.extract(e2r(elseBlock))
    val node: Expr[resTyp.ScalaT] = IfNode(cond.e, thenExpr, elseExpr)
    resTyp.proxy(node)
  }
}

trait AbstractInt extends Common {


  implicit def intProof: scala.Int CanBe Int

  implicit def intTyp: Typ[Int]

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


  lazy val intProof = fromFunction(EvaluationIntProxy)
  implicit val intTyp: Typ[Int] = typ[Int]


  case class EvaluationIntProxy(i: scala.Int) extends IntProxy {
    def +(other: Int) = i + other.i

    def ==(other: Int) = i == other.i

    override def toString = i.toString

  }

  type Int = EvaluationIntProxy

}

trait ExprInt extends CommonExpr with AbstractInt {
  implicit lazy val intProof = fromFunction((x: scala.Int) => ExprIntProxy(Constant(x)))
  implicit val intTyp: Typ[Int] = getProxy[scala.Int, Int]("Int", ExprIntProxy, _.e)

  case class IntPlus(a: Expr[scala.Int], b: Expr[scala.Int]) extends Expr[scala.Int] {
    override def toString = "(" + a.toString + " + " + b.toString + ")"

    def eval() = a.eval() + b.eval()
  }

  case class ExprIntProxy(e: Expr[scala.Int]) extends IntProxy {
    def +(other: Int) = ExprIntProxy(IntPlus(e, other.e))
  }

  type Int = ExprIntProxy
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

trait ExprList extends CommonExpr with AbstractList {

  def listProof[T: Typ] = fromFunction[scala.List[T], List[T]](ExprListProxy.apply)

  def listTyp[T: Typ]: Typ[List[T]] = {
    val tt = implicitly[Typ[T]]
    getProxy[scala.List[T], List[T]](s"List[$tt]", { case Constant(l) => ExprListProxy(l) }, x => Constant(x.l))(listProof)
  }


  // TODO: What if I really want List[Expr[Typ[T]#ScalaT]] (or something like that)
  case class ExprListProxy[+T: Typ](l: scala.List[T]) extends ListProxy[T] {

  }

  type List[+T] = ExprListProxy[T]

  def cons[T: Typ](head: T, tail: List[T]): List[T] = scala.::(head, tail.l)

  def nil: List[Nothing] = ExprListProxy(Nil)
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

trait ExprFold extends CommonExpr with AbstractFold {
  self: ExprList with ExprBoolean with ExprIf with ExprInt =>

  case class Foldr[A, B](k: Expr[(A, B)] => Expr[B], z: Expr[B], xs: List[Expr[A]]) extends Expr[B] {
    def eval() : B = ???
  }

  def foldr[A: Typ, B: Typ](k: (A, B) => B)(z: B)(xs: List[A]): B = ???

  def foldr[A, B, C, D](k: (A, B) => B)(z: B)(xs: List[A])(implicit typA: Typ[A], typB : Typ[B], ac: A CanBe C, bc: B CanBe D): B = {
  ???
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


object ExprTest extends Runner with CommonExpr
  with ExprBoolean with ExprIf with ExprInt with NumericProxies
  with ExprList with ExprFold with InfixListOps {


  def evalProxy[T](expr: T)(implicit typ: Typ[T]): typ.ScalaT = {
    typ.extract(expr).eval()
  }

  def stringProxy[T: Typ](v: T): String = {
    val vTyp = implicitly[Typ[T]]
    vTyp.extract(v).toString
  }

  override def transform[T](subst: Subst)(expr: Expr[T]): Expr[T] = {
    subst.getOrElse(expr, {
      expr match {
        case IfNode(c, t, e) => IfNode(transform(subst)(c), transform(subst)(t), transform(subst)(e))
        case IntPlus(a, b) => IntPlus(transform(subst)(a), transform(subst)(b))
        case BoolOr(a, b) => BoolOr(transform(subst)(a), transform(subst)(b))
        case BoolAnd(a, b) => BoolAnd(transform(subst)(a), transform(subst)(b))
        case Constant(x) => Constant(x)
        case _ => sys.error(s"Dont know how to transform : $expr")
      }
    }).asInstanceOf[Expr[T]] // TODO : can we do without cast ?
  }



  def liftFunctionGood[A, B, C, D](f : A => B)(implicit typA : Typ[A], typB: Typ[B], ca: C CanBe A, db: D CanBe B) : Expr[C => D] = {
    def transformed(c : Expr[typA.ScalaT]) = {
      val argument = typA.proxy(c)
      val result = f(argument)
      typB.extract(result)
    }

    //val t = Id[C]
    Lambda(???, ???)
  }


  def run(): Unit = {
    val intValue = _1 + 2 + 3

    val booleanValue = (_true && false) || true

    val listValue = list(1, 2, 3, 4, 5)

    println(listValue)

    val ifThenElse = _if_then_else(booleanValue)(intValue)(intValue + 14)

    val function = (x: Int) => x + ifThenElse

    val liftedFunction = liftFunction(function)

    println(stringProxy(ifThenElse))
    println(s"Result of the evaluation : ${evalProxy(ifThenElse)}")
    println(function)
    println(liftedFunction)


    // TODO: I don't want to have to do that, why can't the compiler figure it out ?
    val argument = 45.asInstanceOf[ExprTest.intTyp.ScalaT]
    val evaluatedFunction = liftedFunction.eval()
    println(argument)
    println(evaluatedFunction)
    println(s"Evaluation of lifted funciton : ${evaluatedFunction.apply(argument)}")




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

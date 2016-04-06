import scala.language.implicitConversions

object ImplicitHang {

  /* Despite having no explicit recursion, nor loops, this
   * piece of code will hang as the compiler will try to 
   * apply the implicit conversion to type check its own body.
   *
   * This effectifely gets transformed to:
   * implicit def i2s(i: Int): String = i2s(i)
   *
   * Since the call is tail recursive, there will be no 
   * stack overflow, and the program will just hang
   */

  implicit def i2s(i: Int): String = i

  def oneAsString: String = 1 // hangs

  /* At the same time, preventing that behaviour would prevent 
   * people from doing this (rather contrived) example */

  sealed trait Nat
  case object Zero extends Nat
  case class Succ(n: Nat) extends Nat

  implicit def nat2Int(n : Nat): Int = n match {
    case Zero => 0
    case Succ(t) => t + 1
  }

  /* Does not hang */
  def four: Int = Succ(Succ(Succ(Succ(Zero)))) 

}

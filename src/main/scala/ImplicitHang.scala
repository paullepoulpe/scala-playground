import scala.language.implicitConversions

object ImplicitHang {

  /* Despite having no explicit recursion, nor loops, this
   * piece of code will hang as the compiler will try to 
   * apply the implicit conversion to type check it's own body.
   *
   * This effectifely gets transformed to:
   * implicit def i2s(i: Int): String = i2s(i)
   *
   * Since the call is tail recursive, there will be no 
   * stack overflow, and the program will just hang
   */

  implicit def i2s(i: Int): String = i

  def hang: String = 1 // Kill everything
}

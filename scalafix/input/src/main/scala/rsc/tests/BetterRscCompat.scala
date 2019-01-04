/*
rules = "scala:rsc.rules.RscCompat"
RscCompat.better = true
 */
package rsc.tests

import scala.collection.mutable

object BetterRscCompat_Test {
  def x1 = 42
  def x2 = mutable.Map(1 -> "1")

  class CA[A]

  class CB[A](x: A)

  class CC[A](x: Int)

  class CD extends CA

  class CE extends CB

  class CF extends CC(1)

  class CG extends CB(42)

  class CH extends CB[Int](43)

  class CI extends CG

  object nested {
    class CA3[A, B, C](x: A, y: B, z: C)
  }

  class CB3[B](b: B) extends nested.CA3(new CA, b, "z")
}

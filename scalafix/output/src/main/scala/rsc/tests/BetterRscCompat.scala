package rsc.tests

import scala.collection.mutable

object BetterRscCompat_Test {
  def x1: Int = 42
  def x2: mutable.Map[Int, String] = mutable.Map(1 -> "1")

  class CA[A]

  class CB[A](x: A)

  class CC[A](x: Int)

  class CD extends CA[Nothing]

  class CE extends CB[Unit]

  class CF extends CC[Nothing](1)

  class CG extends CB[Int](42)

  class CH extends CB[Int](43)

  class CI extends CG

  object nested {
    class CA3[A, B, C](x: A, y: B, z: C)
  }

  class CB3[B](b: B) extends nested.CA3[CA[Nothing], B, String](new CA, b, "z")
}

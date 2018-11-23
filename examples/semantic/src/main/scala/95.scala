package ticket95

trait X
trait Y

object M {
  def m1: X {} = ???
  def m2: X with Y {} = ???

  def n1: X { def x: Int } = ???
  def n2: X with Y { def x: Int } = ???
}


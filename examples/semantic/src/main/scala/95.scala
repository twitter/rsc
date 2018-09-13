package ticket95

trait X
trait Y

object M {
  def m1: X {} = ???
  def m2: X with Y {} = ???
}


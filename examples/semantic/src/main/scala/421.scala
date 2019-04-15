package ticket421

object C
case class C[T: List](x: Int)

object D {
  def apply(): D[Nothing] = ???
}
case class D[T: List](x: Int)

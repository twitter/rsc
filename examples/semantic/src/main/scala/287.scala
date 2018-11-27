package ticket287

object M {
  implicit class C[M[T]](x: M[Int])
}

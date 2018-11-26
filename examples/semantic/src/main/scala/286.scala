package ticket286

object M {
  implicit class C[T](t: T)(implicit x: List[T])
}

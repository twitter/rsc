package ticket386

object M {
  val c: C = C(42)
  println(c.equals(c))
  println(c.hashCode)
}

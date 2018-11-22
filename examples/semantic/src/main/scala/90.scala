package ticket90

class C {
  class X
}

object M {
  val c: C = new C
  def m: c.X = ???
}

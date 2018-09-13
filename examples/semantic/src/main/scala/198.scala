package ticket198

object Foo extends Enumeration {
  val Bar: Foo.this.Value = Value
}

class C {
  def m1(foo: Foo.Value): Unit = ???
  def m2: scala.List[Int] = ???
}

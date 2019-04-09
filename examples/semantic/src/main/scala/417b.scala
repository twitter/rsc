package ticket417b

object O {
  class C {
    class D
  }
  val c: C = new C
}
class C {
  import O.c.D

  def foo: D = ???
}

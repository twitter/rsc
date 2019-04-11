package ticket417a

object O {
  class C {
    class D
  }
}
class C extends O.C {
  def foo: D = ???
}

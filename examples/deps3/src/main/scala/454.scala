package ticket454

trait C {
  def foo(x: Int = 0): Int
}
object C {
  trait D extends C {
    def foo(x: Int = 0): Int
  }
}

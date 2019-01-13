package ticket312

object M {
  implicit class IntOps(val x: Int) extends AnyVal {
    def m(y: Int): Int = ???
    def m(y: String): Int = ???
    def n: Int = ???
  }
}

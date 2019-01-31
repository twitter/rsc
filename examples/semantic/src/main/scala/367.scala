package ticket367

object M {
  implicit class C[M[_]](x: Int) {
    def m: Int = ???
  }
}

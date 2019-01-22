package ticket337

class C {
  def m: M.D[T] forSome { type T <: Int } = ???
}

object M {
  class D[T]
}

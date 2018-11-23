package ticket275

trait T {
  type X
}

class C {
  type X = x.X
  val x: T = ???
}

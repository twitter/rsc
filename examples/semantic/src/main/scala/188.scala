package ticket188

trait A

trait B {
  type T
}

class D {
  def m(x: A with B): x.T = ???
}

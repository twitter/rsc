package ticket401

trait B1 {
  self: C =>

  def m1: T = ???
  def m2: B1.this.T = ???
}

trait B2 {
  _: C =>

  def m1: T = ???
  def m2: B2.this.T = ???
}

trait C {
  type T
}

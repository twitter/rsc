package ticket399

trait B {
  def m(): Int
}

trait C extends B {
  def m(x: Int): Int
}

trait D extends C {
  def m: Int
}

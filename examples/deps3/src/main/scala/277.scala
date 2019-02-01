package ticket277

trait B {
  def m: Unit
}

trait C extends B {
  override def m: Unit = ???
}

trait D extends B {
  override def m: Unit = ???
}

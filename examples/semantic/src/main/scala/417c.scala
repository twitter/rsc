package ticket417c

trait T {
  class C
}
object T extends T {
  def foo(): C = ???
}

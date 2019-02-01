package ticket101

sealed trait T
class C1 extends T
class C2 extends T
object M extends T

sealed trait U
object U {
  private class D1 extends U
  private class D2 extends U
  private object N extends U
}

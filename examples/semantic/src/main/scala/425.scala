package ticket425

trait T {
  def toString: String
}
trait T2 {
  override def toString: String
}
trait T3 {
  override def toString: String = ""
}
trait T4 {
  def toString(): String
}
trait T5 {
  override def toString(): String
}
trait T6 {
  override def toString(): String = ""
}
abstract class A {
  def toString: String
}
abstract class A2 {
  override def toString: String
}
abstract class A3 {
  override def toString: String = ""
}
abstract class A4 {
  def toString(): String
}
abstract class A5 {
  override def toString(): String
}
abstract class A6 {
  override def toString(): String = ""
}
class C extends T {
  override def toString: String = ""
}
class C2 extends T2 {
  override def toString: String = ""
}
class C3 extends T3 {
  override def toString: String = ""
}
class D extends T {
  override def toString(): String = ""
}
class E {
  override def toString: String = ""
}
class F extends A {
  override def toString: String = ""
}

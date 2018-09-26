package ticket224

import scala.collection._

class A1[T]
trait B1[T] extends A1[T]
class C1 extends B1[Int]

abstract class C2 extends AbstractMap[Int, Int]

trait B3[K, V] extends AbstractMap[K, V]
abstract class C3 extends AbstractMap[Int, Int]

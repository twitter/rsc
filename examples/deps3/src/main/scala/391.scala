package ticket391

import scala.language.higherKinds

class C[M[_]] {
  def m: C[({ type L[T] = List[T] })#L] = ???
  def n: M[Int] = ???
}

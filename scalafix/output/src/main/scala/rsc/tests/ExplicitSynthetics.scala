package rsc.tests

class ExplicitSynthetics_Test {

  val a = List.apply[_root_.scala.Int](1, 2, 3)

  List(1).map(_ + 2)(_root_.scala.collection.immutable.List.canBuildFrom[_root_.scala.Int])
  Array.empty[Int](_root_.scala.reflect.`package`.materializeClassTag).headOption
  _root_.scala.Predef.augmentString("fooo").stripPrefix("o")

  val Name = _root_.scala.Predef.augmentString("name:(.*)").r
  val x #:: xs = Stream.apply[_root_.scala.Int](1, 2)
  val Name(name) = "name:foo"
  1 #:: _root_.scala.collection.immutable.Stream.consWrapper[_root_.scala.Int](2 #:: Stream.empty)

  val lst = 1 #:: _root_.scala.collection.immutable.Stream.consWrapper[_root_.scala.Int](2 #:: Stream.empty)
  _root_.scala.Predef.any2stringadd[_root_.scala.collection.immutable.Stream[_root_.scala.Int]](lst) + "foo"

  (1 to 10).foreach[_root_.scala.Unit]({x => (0 until 10).foreach[_root_.scala.Unit]({y => println(x -> x)})})
  (1 to 10).flatMap[_root_.scala.Tuple2[_root_.scala.Int, _root_.scala.Int], _root_.scala.collection.immutable.IndexedSeq[_root_.scala.Tuple2[_root_.scala.Int, _root_.scala.Int]]]({i => (0 until 10).map[_root_.scala.Tuple2[_root_.scala.Int, _root_.scala.Int], _root_.scala.collection.immutable.IndexedSeq[_root_.scala.Tuple2[_root_.scala.Int, _root_.scala.Int]]]({j => (i, j)})(_root_.scala.collection.immutable.IndexedSeq.canBuildFrom[_root_.scala.Tuple2[_root_.scala.Int, _root_.scala.Int]])})(_root_.scala.collection.immutable.IndexedSeq.canBuildFrom[_root_.scala.Tuple2[_root_.scala.Int, _root_.scala.Int]])
  (1 to 10).flatMap[_root_.scala.Tuple2[_root_.scala.Int, _root_.scala.Int], _root_.scala.collection.immutable.IndexedSeq[_root_.scala.Tuple2[_root_.scala.Int, _root_.scala.Int]]]({i => (0 until 10).withFilter({j => i % 2 == 0}).map[_root_.scala.Tuple2[_root_.scala.Int, _root_.scala.Int], _root_.scala.collection.immutable.IndexedSeq[_root_.scala.Tuple2[_root_.scala.Int, _root_.scala.Int]]]({j => (i, j)})(_root_.scala.collection.immutable.IndexedSeq.canBuildFrom[_root_.scala.Tuple2[_root_.scala.Int, _root_.scala.Int]])})(_root_.scala.collection.immutable.IndexedSeq.canBuildFrom[_root_.scala.Tuple2[_root_.scala.Int, _root_.scala.Int]])

  object s {
    def apply() = 2
    s.apply()
    s.apply()
    case class Bar()
    Bar.apply()
    null.asInstanceOf[Int => Int].apply(2)
  }

  class J[T: Manifest] { val arr = Array.empty[T](J.this.evidence$1) }

  class F
  implicit val ordering: Ordering[F] = ???
  val f: Ordered[F] = _root_.scala.math.Ordered.orderingToOrdered[ExplicitSynthetics_Test.this.F](new F)(ExplicitSynthetics_Test.this.ordering)

  import scala.concurrent.ExecutionContext.Implicits.global
  scala.concurrent.Future.successful(1).foreach[_root_.scala.Unit]({a => scala.concurrent.Future.successful(2).foreach[_root_.scala.Unit]({b => println(a)})(_root_.scala.concurrent.ExecutionContext.Implicits.global)})(_root_.scala.concurrent.ExecutionContext.Implicits.global)
  scala.concurrent.Future.successful(1).flatMap[_root_.scala.Int]({a => scala.concurrent.Future.successful(2).withFilter({b => a < b})(_root_.scala.concurrent.ExecutionContext.Implicits.global).map[_root_.scala.Int]({b => a})(_root_.scala.concurrent.ExecutionContext.Implicits.global)})(_root_.scala.concurrent.ExecutionContext.Implicits.global)

}

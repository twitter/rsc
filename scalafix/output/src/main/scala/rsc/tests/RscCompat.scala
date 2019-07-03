package rsc.tests

import com.twitter.bijection._
import java.util.Base64
import scala.language.existentials
import scala.language.higherKinds

object RscCompat_Test {
  class Basic {
    def x1: _root_.scala.Int = 42
    val x2: _root_.java.lang.String = ""
    final val x3 = ""
    var x4: _root_.java.lang.String = ""
  }

  class Patterns {
    val List() = List()
    val List((x2: _root_.scala.Int)) = List(2)
    val List((x3: _root_.scala.Int), (y3: _root_.scala.Int)) = List(3, 3)
    val x4, y4: _root_.scala.Int = 4
    val (x9: _root_.scala.Int) :: (y9: _root_.scala.collection.immutable.List[_root_.scala.Int]) = List(9, 9, 9)
    var List() = List()
    var List((x6: _root_.scala.Int)) = List(6)
    var List((x7: _root_.scala.Int), (y7: _root_.scala.Int)) = List(7, 7)
    var x8, y8: _root_.scala.Int = 8
    var (x10: _root_.scala.Int) :: (y10: _root_.scala.collection.immutable.List[_root_.scala.Int]) = List(10, 10, 10)
  }

  class Visibility {
    private def x1 = ""
    private[this] def x2 = ""
    private[rsc] def x3: _root_.java.lang.String = ""
    protected def x4: _root_.java.lang.String = ""
    protected[this] def x5: _root_.java.lang.String = ""
    protected[rsc] def x6: _root_.java.lang.String = ""
  }

  private class Private { def x1 = "" }
  private[this] class PrivateThis { def x1 = "" }
  private[rsc] class PrivateRsc { def x1: _root_.java.lang.String = "" }
  protected class Protected { def x1: _root_.java.lang.String = "" }
  protected[this] class ProtectedThis { def x1: _root_.java.lang.String = "" }
  protected[rsc] class ProtectedRsc { def x1: _root_.java.lang.String = "" }

  private trait PrivateTrait { def x1 = "" }
  trait PublicTrait {
    private def x1: _root_.java.lang.String = ""
    private[this] def x2: _root_.java.lang.String = ""
    private[rsc] def x3: _root_.java.lang.String = ""
    protected def x4: _root_.java.lang.String = ""
    protected[this] def x5: _root_.java.lang.String = ""
    protected[rsc] def x6: _root_.java.lang.String = ""
  }

  object Config {
    val x: Int = 1
    val y = 2
  }

  object TypesHelpers {
    class C
    class E {
      class C
    }
    class P {
      class C
      val c: C = ???
    }
    val p: _root_.rsc.tests.RscCompat_Test.TypesHelpers.P = new P
    val c: _root_.rsc.tests.RscCompat_Test.TypesHelpers.p.C = p.c
    trait A
    trait B
    class ann extends scala.annotation.StaticAnnotation
    class H[M[_]]
  }

  trait TypesBase {
    class X
    val x: X = ???
  }

  class Types[T] extends TypesBase {
    import TypesHelpers._
    override val x: X = new X

    val typeRef1: _root_.rsc.tests.RscCompat_Test.TypesHelpers.C = ??? : C
    val typeRef2: _root_.rsc.tests.RscCompat_Test.TypesHelpers.p.C = ??? : p.C
    val typeRef3: _root_.rsc.tests.RscCompat_Test.TypesHelpers.E#C = ??? : E#C
    val typeRef4: _root_.scala.`package`.List[_root_.scala.Int] = ??? : List[Int]
    val typeRef5: Types.this.X = ??? : X
    val typeRef6: T = ??? : T
    val typeRef7: _root_.scala.Function0[T] = ??? : () => T
    val typeRef8: _root_.scala.Function1[T, T] = ??? : T => T
    val typeRef9: _root_.scala.Function2[T, T, T] = ??? : (T, T) => T
    val typeRef10: (T, T) = ??? : (T, T)

    val singleType1: _root_.rsc.tests.RscCompat_Test.TypesHelpers.c.type = ??? : c.type
    val singleType2: _root_.rsc.tests.RscCompat_Test.TypesHelpers.p.c.type = ??? : p.c.type
    val Either: _root_.scala.util.Either.type = ??? : scala.util.Either.type

    val thisType1: Types.this.type = ??? : this.type
    val thisType2: Types.this.type = ??? : Types.this.type

    // FIXME: https://github.com/twitter/rsc/issues/143
    // val superType1 = ??? : super.x.type
    // val superType2 = ??? : super[TypesBase].x.type
    // val superType3 = ??? : Types.super[TypesBase].x.type

    // FIXME: https://github.com/twitter/rsc/issues/145
    // val constantType1 = ??? : 42.type

    val compoundType1: _root_.scala.AnyRef { def k: _root_.scala.Int } = ??? : { def k: Int }
    val compoundType2: _root_.rsc.tests.RscCompat_Test.TypesHelpers.A with _root_.rsc.tests.RscCompat_Test.TypesHelpers.B = ??? : A with B
    val compoundType3: _root_.rsc.tests.RscCompat_Test.TypesHelpers.A with _root_.rsc.tests.RscCompat_Test.TypesHelpers.B { def k: _root_.scala.Int } = ??? : A with B { def k: Int }
    val compoundType4: _root_.scala.AnyRef { def k: _root_.scala.Int } = new { def k: Int = ??? }
    val compoundType5: _root_.scala.AnyRef with _root_.rsc.tests.RscCompat_Test.TypesHelpers.A with _root_.rsc.tests.RscCompat_Test.TypesHelpers.B = new A with B
    val compoundType6: _root_.scala.AnyRef with _root_.rsc.tests.RscCompat_Test.TypesHelpers.A with _root_.rsc.tests.RscCompat_Test.TypesHelpers.B { def k: _root_.scala.Int } = new A with B { def k: Int = ??? }
    val compoundType7: _root_.rsc.tests.RscCompat_Test.TypesHelpers.A with (_root_.scala.collection.immutable.List[T] forSome { type T }) with _root_.rsc.tests.RscCompat_Test.TypesHelpers.B = ??? : A with (List[T] forSome { type T }) with B

    // FIXME: https://github.com/twitter/rsc/issues/317
    // val annType1 = ??? : C @ann

    val existentialType1: _root_.scala.Any = ??? : T forSome { type T }
    val existentialType2: _root_.scala.collection.immutable.List[_root_.scala.Any] = ??? : List[_]

    val universalType1: _root_.rsc.tests.RscCompat_Test.TypesHelpers.H[({ type λ[U] = _root_.scala.collection.immutable.List[U] })#λ] = ??? : H[({ type L[U] = List[U] })#L]

    val byNameType: (=> _root_.scala.Any) => _root_.scala.Any = ??? : ((=> Any) => Any)
    // FIXME: https://github.com/twitter/rsc/issues/144
    // val repeatedType = ??? : ((Any*) => Any)
  }

  implicit val implicit_x: Int = 42
  implicit val implicit_y: String = "42"
  trait In
  trait Out1
  trait Out2
  implicit val implicit_bijection1: ImplicitBijection[In, Out1] = ???
  implicit val implicit_bijection2: ImplicitBijection[In, Out2] = ???

  class Bugs {
    val Either: _root_.scala.util.Either.type = scala.util.Either

    implicit def order[T]: _root_.java.lang.Object with _root_.scala.`package`.Ordering[_root_.scala.`package`.List[T]] = new Ordering[List[T]] {
      def compare(a: List[T], b: List[T]): Int = ???
    }

    val gauges: _root_.scala.collection.immutable.List[_root_.scala.Int] = {
      val local1 = 42
      val local2 = 43
      List(local1, local2)
    }

    val clazz: _root_.java.lang.Class[T1] forSome { type T1 } = Class.forName("foo.Bar")

    val compound: _root_.scala.AnyRef { def m(x: _root_.scala.Int): _root_.scala.Int } = ??? : { def m(x: Int): Int }

    val loaded: _root_.scala.collection.immutable.List[_root_.java.lang.Class[T1] forSome { type T1 }] = List[Class[_]]()

    val ti: Types[Int] = ???
    val innerClass1: _root_.rsc.tests.RscCompat_Test.Types[_root_.scala.Predef.String]#X = new Types[String]().x
    val innerClass2: _root_.rsc.tests.RscCompat_Test.Types[_root_.scala.Int]#X = ??? : Types[Int]#X
    val innerClass3: Bugs.this.ti.X = ti.x
    val innerClass4: _root_.java.util.Base64.Decoder = Base64.getMimeDecoder

    val param: _root_.scala.AnyRef { val default: _root_.scala.Boolean } = new { lazy val default = true }
    val more1: _root_.scala.AnyRef { var x: _root_.scala.Int;  } = new { var x = 42 }
    val more2: _root_.scala.AnyRef { def foo(implicit x: _root_.scala.Int, y: _root_.scala.Int): _root_.scala.Int } = new { def foo(implicit x: Int, y: Int) = 42 }
    val more3: _root_.scala.AnyRef { def bar: _root_.scala.Int } = new { implicit def bar = 42 }

    implicit val crazy1 = implicitly[Int]
    implicit val crazy2 = Bijection.connect[In, Out1]
    val sane1: _root_.scala.Predef.String = implicitly[String]
    val sane2: _root_.com.twitter.bijection.Bijection[_root_.rsc.tests.RscCompat_Test.In, _root_.rsc.tests.RscCompat_Test.Out2] = Bijection.connect[In, Out2]

    // FIXME: https://github.com/scalameta/scalameta/issues/1872#issuecomment-498705622
    // val X, List(y) = List(1, 2)

    val t: _root_.rsc.tests.foo.`package`.T = ??? : foo.T

    val (a: String, b: String, (c: _root_.java.lang.String)) = ("foo", "bar", "baz")
  }
}

private class RscCompat_Test {
  def x1: _root_.java.lang.String = ""
}

package object foo {
  class T
}

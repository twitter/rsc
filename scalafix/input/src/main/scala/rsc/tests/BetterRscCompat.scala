/*
rules = "scala:rsc.rules.RscCompat"
 */
package rsc.tests

import rsc.tests.BetterRscCompat_Test.AutoImport.oa.ob.od
import testpkg._

object BetterRscCompat_Test {

  object ShortenInferredType {

    import scala.collection.mutable

    def int = 42
    def map = mutable.Map(1 -> "1")
    def str = "hello"
    def str2 = s"$str world"

    class MyClass

    val obj = new MyClass
  }

  object InferInitTypeArgs {

    class CA[A]

    class CB[A](x: A)

    class CC[A](x: Int)

    class CD extends CA

    class CE extends CB

    class CF extends CC(1)

    class CG extends CB(42)

    class CH extends CB[Int](43)

    class CI extends CG

    object nested {
      class CA3[A, B, C](x: A, y: B, z: C)
    }

    class CB3[B](b: B) extends nested.CA3(new CA, b, "z")

    class CC3[B](b: B) extends nested.CA3[Int, Int, Int](1, 2, 3)
  }

  object AutoImport {

    object oa {
      class MyClass
      object ob {
        object od {
          class MyClass
        }
      }
      object oc {
        object od {
          class MyClass

          import scala.collection.mutable

          val map = mutable.Map(new MyClass -> new oe.MyClass2)
        }
      }
      object oe {
        class MyClass2 {
          class Inner
        }
      }
      object of {
        class MyClass2

        val mc2 = new MyClass2
      }
    }

    val x = new oa.MyClass

    val y = new od.MyClass

    val z = oa.oc.od.map

    private object po {
      val v1 = new oa.oe.MyClass2
      val w1 = new v1.Inner
    }
    val v2 = new oa.oe.MyClass2
    val w2 = new v2.Inner
  }

  object ClassLinearization {

    trait MyTrait1 {
      class MyInnerClass1

      def foo = new MyInnerClass1
    }

    trait MyTrait2 extends MyTrait1

    object myobj extends MyTrait2 {
      def bar = foo
    }

    val baz = myobj.bar

    object myError extends Exception {
      def foo = 1
    }
  }

  object AnyrefWithTrait {

    trait Trait1 {
      def foo: Int
    }
    trait Trait2 {
      val bar: Int
    }

    val t1 = new Trait1 {
      def foo: Int = 1
    }

    val t2 = new Trait1 with Trait2 {
      def foo: Int = 1
      val bar: Int = 1
    }

    val t3 = new Trait2 {
      def foo: Int = 1
      val bar: Int = 1
    }

    val tSeq = Seq(new Trait1 { def foo = 1 })

    val anonObj = new {}

    val anonObj2 = new {} with Trait2 {
      val bar = 2
    }

    val anonObj3 = new { val bar: Int = 3 } with Trait2

    def order[T] = new Ordering[T] {
      override def compare(x: T, y: T): Int = 0
    }
  }

  object TypeAliases {

    object scope1 {
      class MyClass

      def foo: MyClass = null
    }

    object scope2 {
      type MyClass = scope1.MyClass

      def foo: MyClass = null
    }

    object right2left {
      import scope1.MyClass

      val bar = scope2.foo
    }

    object left2right {
      import scope2.MyClass

      val bar = scope1.foo
    }
  }

  object PackageObjects {
    val o2c = O1.foo

    val poc = new PkgObjClass
  }
}

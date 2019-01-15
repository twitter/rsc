package rsc.tests

import rsc.tests.BetterRscCompat_Test.AutoImport.oa.ob.od
import rsc.tests.BetterRscCompat_Test.AutoImport.oa.{MyClass, oc}
import rsc.tests.BetterRscCompat_Test.AutoImport.oa.oe.MyClass2
import scala.collection.{Seq, mutable}

object BetterRscCompat_Test {

  object ShortenInferredType {

    import scala.collection.mutable

    def int: Int = 42
    def map: mutable.Map[Int, String] = mutable.Map(1 -> "1")
    def str: String = "hello"
    def str2: String = s"$str world"

    class MyClass

    val obj: MyClass = new MyClass
  }

  object InferInitTypeArgs {

    class CA[A]

    class CB[A](x: A)

    class CC[A](x: Int)

    class CD extends CA[Nothing]

    class CE extends CB[Unit]

    class CF extends CC[Nothing](1)

    class CG extends CB[Int](42)

    class CH extends CB[Int](43)

    class CI extends CG

    object nested {
      class CA3[A, B, C](x: A, y: B, z: C)
    }

    class CB3[B](b: B) extends nested.CA3[CA[Nothing], B, String](new CA, b, "z")

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

          val map: mutable.Map[MyClass, MyClass2] = mutable.Map(new MyClass -> new oe.MyClass2)
        }
      }
      object oe {
        class MyClass2 {
          class Inner
        }
      }
      object of {
        class MyClass2

        val mc2: MyClass2 = new MyClass2
      }
    }

    val x: MyClass = new oa.MyClass

    val y: od.MyClass = new od.MyClass

    val z: mutable.Map[oc.od.MyClass, MyClass2] = oa.oc.od.map

    private object po {
      val v1 = new oa.oe.MyClass2
      val w1 = new v1.Inner
    }
    val v2: MyClass2 = new oa.oe.MyClass2
    val w2: v2.Inner = new v2.Inner
  }

  object ClassLinearization {

    trait MyTrait1 {
      class MyInnerClass1

      def foo: MyInnerClass1 = new MyInnerClass1
    }

    trait MyTrait2 extends MyTrait1

    object myobj extends MyTrait2 {
      def bar: MyInnerClass1 = foo
    }

    val baz: myobj.MyInnerClass1 = myobj.bar

    object myError extends Exception {
      def foo: Int = 1
    }
  }

  object AnyrefWithTrait {

    trait Trait1 {
      def foo: Int
    }
    trait Trait2 {
      val bar: Int
    }

    val t1: Trait1 = new Trait1 {
      def foo: Int = 1
    }

    val t2: Trait1 with Trait2 = new Trait1 with Trait2 {
      def foo: Int = 1
      val bar: Int = 1
    }

    val t3: Trait2 { def foo: Int } = new Trait2 {
      def foo: Int = 1
      val bar: Int = 1
    }

    val tSeq: Seq[Trait1] = Seq(new Trait1 { def foo = 1 })

    val anonObj: AnyRef = new {}

    val anonObj2: Trait2 = new {} with Trait2 {
      val bar = 2
    }

    val anonObj3: Trait2 = new { val bar: Int = 3 } with Trait2

    def order[T]: Ordering[T] = new Ordering[T] {
      override def compare(x: T, y: T): Int = 0
    }
  }
}

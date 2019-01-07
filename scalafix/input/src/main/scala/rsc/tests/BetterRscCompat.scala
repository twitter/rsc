/*
rules = "scala:rsc.rules.RscCompat"
RscCompat.better = true
 */
package rsc.tests

import rsc.tests.BetterRscCompat_Test.AutoImport.oa.ob.od

object BetterRscCompat_Test {

  object ShortenInferredType {

    import scala.collection.mutable

    def int = 42
    def map = mutable.Map(1 -> "1")
    def str = "hello"

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
}

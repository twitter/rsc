package ticket410

trait T[A]

class C[A](a: A) extends T[Int] {
  def this() = this(???)
}

class D extends C(1)

class E extends C

private class PD extends C(2)

abstract class AD extends C(3)

class F(x: String) extends C(x)

private class PD2(x: String) extends C(x)

abstract class AD2 extends C(4)

class G extends F("")

class DA extends CA(5)

class DB extends CB(6)

private class PDB extends CB(7)

abstract class ADB extends CB(8)

class DA2 extends ticket410.CA(9)

class DB2 extends ticket410.CB(10)

class DB3 extends CB[Int](11)

class DB4 extends ticket410.CB[Int](12)

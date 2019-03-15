package ticket408

class C {

  val x = ""

  protected def y = 42

  def z: Int = 43

  private val x2 = ""

  private[this] def y2 = 44

  trait T[A]

  class D[A](a: A) extends T[Int] {
    def this() = this(???)
  }

  class E extends D(1)

}

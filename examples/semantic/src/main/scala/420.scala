case class C(implicit val x: Int)

case class C2[A](implicit val x: Int)

case class C3[A](implicit val x: A)

case class C4[A](implicit val x: A, val y: Int)

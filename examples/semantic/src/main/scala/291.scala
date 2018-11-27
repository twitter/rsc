package ticket291

object M {
  implicit def m: Int = ???
}

class C {
  import M._
  implicit val x = implicitly[Int]
}

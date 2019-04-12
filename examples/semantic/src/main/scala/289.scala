package ticket289

case class C[T: List](x: Int)

case class C2[T: List: Option](x: Int)(y: String)

case class C3[T: List: Option](x: T)

case class C4[T: List: Option](x: T, y: Int)(z: String)

case class C5[T: List: Option]()

case class C6[T: List: Option]()()

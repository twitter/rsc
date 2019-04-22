package ticket289
// A context bound like this should desugar into
// case class C[T](x: Int)(implicit evidence$1: List[T])
case class C[T: List](x: Int)

case class C2[T: List: Option](x: Int)(y: String)

case class C3[T: List: Option](x: T)

case class C4[T: List: Option](x: T, y: Int)(z: String)

case class C5[T: List: Option]()

case class C6[T: List: Option]()()

case class C7[T >: Unit <: AnyVal : List: Option](x: T)

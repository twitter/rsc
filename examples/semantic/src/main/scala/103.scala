package ticket103

class NullaryOutlineScalaDef {
  def nullary: Int = ???
}

class NonNullaryOutlineScalaDef {
  def nonNullary(): Int = ???
}

class OutlineVal {
  val nullary: Int = ???
}

abstract class NullaryDefOverridesJavaDef extends java.util.Collection[String] {
  override def size: Int = ???
}

abstract class NullaryDefOverridesNullaryClasspathScalaDef extends scala.ref.Reference[String] {
  override def get: Option[String] = ???
}

class NullaryDefOverridesNonNullaryClasspathScalaDef {
  override def toString: String = ???
}

class NullaryDefOverridesNullaryOutlineScalaDef extends NullaryOutlineScalaDef {
  override def nullary: Int = ???
}

class NullaryDefOverridesNonNullaryOutlineScalaDef extends NonNullaryOutlineScalaDef {
  override def nonNullary: Int = ???
}

abstract class ValOverridesJavaDef extends java.util.Collection[String] {
  override val size: Int = ???
}

abstract class ValOverridesNullaryClasspathScalaDef extends scala.ref.Reference[String] {
  override val get: Option[String] = ???
}

class ValOverridesNonNullaryClasspathScalaDef {
  override val toString: String = ???
}

class ValOverridesNullaryOutlineScalaDef extends NullaryOutlineScalaDef {
  override val nullary: Int = ???
}

class ValOverridesNonNullaryOutlineScalaDef extends NonNullaryOutlineScalaDef {
  override val nonNullary: Int = ???
}

abstract class ValOverridesClasspathVal extends scala.reflect.AnyValManifest[Int]("") {
  override val toString: String = ???
}

class ValOverridesOutlineVal extends OutlineVal {
  override val nullary: Int = ???
}

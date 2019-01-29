// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
// NOTE: This file has been partially copy/pasted from scalameta/scalameta.
package rsc.classpath.javacp

import rsc.classpath.javacp.asm._
import org.objectweb.asm.signature._

/** Translation of "Signature" section from the JVM spec to Scala.
 *
 * @see https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.7.9.1
 */
sealed trait JavaTypeSignature extends Printable
object JavaTypeSignature {

  /** Parse JVM signature using a custom traverser.
   *
   * Example:
   *
   * {{{
   *   parse("...", new ClassSignatureVisitor)
   * }}}
   */
  final def parse[T](signature: String, visitor: TypedSignatureVisitor[T]): T = {
    val signatureReader = new SignatureReader(signature)
    signatureReader.accept(visitor)
    visitor.result()
  }
}

// NOTE: We use Scala-friendly names for base types instead of the Java primitive keywords.
abstract class BaseType(val name: String) extends JavaTypeSignature with Product {
  final override def print(sb: StringBuilder): Unit =
    sb.append(this.productPrefix)
}
object BaseType {
  case object B extends BaseType("Byte")
  case object C extends BaseType("Char")
  case object D extends BaseType("Double")
  case object F extends BaseType("Float")
  case object I extends BaseType("Int")
  case object J extends BaseType("Long")
  case object S extends BaseType("Short")
  case object Z extends BaseType("Boolean")

  // JVMS deviation. The void descriptor is not part of BaseType
  // but having it here simplifies things.
  case object V extends BaseType("Unit")
}

sealed trait ReferenceTypeSignature extends JavaTypeSignature

case class ClassTypeSignature(
    simpleClassTypeSignature: SimpleClassTypeSignature,
    classTypeSignatureSuffix: List[ClassTypeSignatureSuffix]
) extends ReferenceTypeSignature
    with ThrowsSignature {
  override def print(sb: StringBuilder): Unit = {
    sb.append('L')
    simpleClassTypeSignature.print(sb)
    classTypeSignatureSuffix.foreach(_.print(sb))
    sb.append(';')
  }
}
object ClassTypeSignature {
  def simple(name: String): ClassTypeSignature =
    ClassTypeSignature(
      SimpleClassTypeSignature(name, None),
      Nil
    )
}

case class TypeVariableSignature(identifier: String)
    extends ReferenceTypeSignature
    with ThrowsSignature {
  override def print(sb: StringBuilder): Unit = {
    sb.append('T')
    sb.append(identifier)
    sb.append(';')
  }
}

case class ArrayTypeSignature(javaTypeSignature: JavaTypeSignature) extends ReferenceTypeSignature {
  override def print(sb: StringBuilder): Unit = {
    sb.append("[")
    javaTypeSignature.print(sb)
  }
}

// JVMS deviation. There is no PackageSpecifier, we encode the path in the
// identifier instead because that's what we get from ASM.
case class SimpleClassTypeSignature(identifier: String, typeArguments: Option[TypeArguments])
    extends Printable {
  require(identifier != null)
  def print(sb: StringBuilder): Unit = {
    sb.append(identifier)
    typeArguments match {
      case Some(t: TypeArguments) =>
        sb.append('<')
        t.print(sb)
        sb.append('>')
      case None =>
    }
  }
}

case class TypeArguments(head: TypeArgument, tail: List[TypeArgument]) extends Printable {
  def all: List[TypeArgument] = head :: tail
  override def print(sb: StringBuilder): Unit = {
    head.print(sb)
    tail.foreach(_.print(sb))
  }
}

trait TypeArgument extends Printable
case object WildcardTypeArgument extends TypeArgument {
  override def print(sb: StringBuilder): Unit = sb.append('*')
}
case class ReferenceTypeArgument(
    wildcard: Option[WildcardIndicator],
    referenceTypeSignature: ReferenceTypeSignature)
    extends TypeArgument {
  override def print(sb: StringBuilder): Unit = {
    wildcard match {
      case Some(w: WildcardIndicator) => w.print(sb)
      case _ =>
    }
    referenceTypeSignature.print(sb)
  }
}

sealed class WildcardIndicator(wildcard: Char) extends Printable {
  override def print(sb: StringBuilder): Unit = sb.append(wildcard)
}
object WildcardIndicator {
  case object Plus extends WildcardIndicator('+')
  case object Minus extends WildcardIndicator('-')
  // NOTE: * is strictly not a WildcardIndicator, however having
  // star here makes it easier to implement TypeArgumentVisitor.
  case object Star extends WildcardIndicator('*')
}
case class ClassTypeSignatureSuffix(simpleClassTypeSignature: SimpleClassTypeSignature)
    extends Printable {
  override def print(sb: StringBuilder): Unit = {
    sb.append('.')
    simpleClassTypeSignature.print(sb)
  }
}

case class ClassSignature(
    typeParameters: Option[TypeParameters],
    superclassSignature: ClassTypeSignature,
    superinterfaceSignatures: List[ClassTypeSignature])
    extends Printable {
  def parents: List[ClassTypeSignature] = superclassSignature :: superinterfaceSignatures
  override def print(sb: StringBuilder): Unit = {
    typeParameters match {
      case Some(tp: TypeParameters) =>
        tp.print(sb)
      case _ =>
    }
    superclassSignature.print(sb)
    superinterfaceSignatures.foreach(_.print(sb))
  }
}
object ClassSignature {
  def simple(superClass: String, interfaces: List[String]): ClassSignature = {
    ClassSignature(
      None,
      ClassTypeSignature.simple(superClass),
      interfaces.map(ClassTypeSignature.simple)
    )
  }
}

case class TypeParameters(head: TypeParameter, tail: List[TypeParameter]) extends Printable {
  def all: List[TypeParameter] = head :: tail
  override def print(sb: StringBuilder): Unit = {
    sb.append('<')
    head.print(sb)
    tail.foreach(_.print(sb))
    sb.append('>')
  }
}

case class TypeParameter(
    identifier: String,
    classBound: ClassBound,
    interfaceBounds: List[InterfaceBound])
    extends Printable {
  def upperBounds: List[ReferenceTypeSignature] = classBound match {
    case ClassBound(Some(sig)) => sig :: interfaceBounds.map(_.referenceTypeSignature)
    case ClassBound(_) => interfaceBounds.map(_.referenceTypeSignature)
  }
  override def print(sb: StringBuilder): Unit = {
    sb.append(identifier)
    classBound.print(sb)
    interfaceBounds.foreach(_.print(sb))
  }
}

case class ClassBound(referenceTypeSignature: Option[ReferenceTypeSignature]) extends Printable {
  override def print(sb: StringBuilder): Unit = {
    sb.append(':')
    referenceTypeSignature match {
      case Some(r: ReferenceTypeSignature) => r.print(sb)
      case _ =>
    }
  }
}

case class InterfaceBound(referenceTypeSignature: ReferenceTypeSignature) extends Printable {
  override def print(sb: StringBuilder): Unit = {
    sb.append(':')
    referenceTypeSignature.print(sb)
  }
}

case class MethodSignature(
    typeParameters: Option[TypeParameters],
    params: List[JavaTypeSignature],
    result: JavaTypeSignature,
    throws: List[ThrowsSignature])
    extends Printable {
  override def print(sb: StringBuilder): Unit = {
    typeParameters match {
      case Some(tp: TypeParameters) =>
        tp.print(sb)
      case _ =>
    }
    sb.append('(')
    params.foreach { param =>
      param.print(sb)
    }
    sb.append(')')
    result.print(sb)
    throws.foreach(_.print(sb))

  }
}

// Included for completeness, currently not used by javacp.
trait ThrowsSignature extends Printable
object ThrowsSignature {
  case class ClassType(classTypeSignature: ClassTypeSignature) extends ThrowsSignature {
    override def print(sb: StringBuilder): Unit = {
      sb.append('^')
      classTypeSignature.print(sb)
    }
  }
  case class TypeVariable(typeVariableSignature: TypeVariableSignature) extends ThrowsSignature {
    override def print(sb: StringBuilder): Unit = {
      sb.append('^')
      typeVariableSignature.print(sb)
    }
  }
}

// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
// NOTE: This file has been partially copy/pasted from scalameta/scalameta.
package rsc.classpath.javacp.asm

import rsc.classpath.javacp._
import org.objectweb.asm.signature.SignatureVisitor

class ReferenceTypeSignatureVisitor extends TypedSignatureVisitor[Option[ReferenceTypeSignature]] {
  private var arrayTypeSignatureVisitor: JavaTypeSignatureVisitor = _
  private var typeVariable: TypeVariableSignature = _
  private val simpleClassTypeSignatures = List.newBuilder[SimpleClassTypeSignatureBuilder]
  private var lastSimpleClassTypeSignatures: SimpleClassTypeSignatureBuilder = _
  def classTypeSignature(): ClassTypeSignature = result().get.asInstanceOf[ClassTypeSignature]
  override def result(): Option[ReferenceTypeSignature] = {
    if (arrayTypeSignatureVisitor != null) {
      arrayTypeSignatureVisitor.result() match {
        case r: ReferenceTypeSignature => Some(r)
        case baseType =>
          throw new IllegalArgumentException(s"Expected Reference Type, obtained $baseType")
      }
    } else if (typeVariable != null) {
      Some(typeVariable)
    } else {
      simpleClassTypeSignatures.result() match {
        case Nil => None
        case simpleClass :: suffix =>
          Some(
            ClassTypeSignature(
              simpleClass.result(),
              suffix.map(s => ClassTypeSignatureSuffix(s.result()))
            )
          )
      }
    }
  }

  override def visitSuperclass(): SignatureVisitor = this

  override def visitArrayType(): SignatureVisitor = {
    val visitor = new JavaTypeSignatureVisitor(isArray = true)
    arrayTypeSignatureVisitor = visitor
    visitor
  }

  def startSimpleClass(name: String): Unit = {
    require(name != null)
    val builder = new SimpleClassTypeSignatureBuilder(name)
    simpleClassTypeSignatures += builder
    lastSimpleClassTypeSignatures = builder
  }

  override def visitClassType(name: String): Unit = {
    name match {
      // ASM does not strip off the L for java.lang.Object for some reason.
      case "Ljava/lang/Object" => startSimpleClass("java/lang/Object")
      case _ => startSimpleClass(name)
    }
  }

  override def visitInnerClassType(name: String): Unit = {
    startSimpleClass(name)
  }

  override def visitTypeArgument(wildcard: Char): SignatureVisitor = {
    lastSimpleClassTypeSignatures.visitTypeArgument(wildcard)
  }

  override def visitTypeArgument(): Unit = {
    lastSimpleClassTypeSignatures.visitTypeArgument()
  }

  override def visitTypeVariable(name: String): Unit = {
    typeVariable = TypeVariableSignature(name)
  }

}

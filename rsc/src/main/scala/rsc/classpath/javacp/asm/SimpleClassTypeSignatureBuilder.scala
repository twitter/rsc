// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
// NOTE: This file has been partially copy/pasted from scalameta/scalameta.
package rsc.classpath.javacp.asm

import rsc.classpath.javacp._
import org.objectweb.asm.signature.SignatureVisitor

class SimpleClassTypeSignatureBuilder(identifier: String) {
  require(identifier != null)
  private val typeArguments = List.newBuilder[TypeArgumentVisitor]
  def result(): SimpleClassTypeSignature =
    typeArguments.result() match {
      case Nil => SimpleClassTypeSignature(identifier, None)
      case head :: tail =>
        SimpleClassTypeSignature(
          identifier,
          Some(TypeArguments(head.result(), tail.map(_.result()))))
    }

  def visitTypeArgument(): Unit = {
    val typeArgumentVisitor = new TypeArgumentVisitor
    typeArguments += typeArgumentVisitor
    typeArgumentVisitor.visitTypeArgument()
  }

  def visitTypeArgument(wildcard: Char): SignatureVisitor = {
    val typeArgumentVisitor = new TypeArgumentVisitor
    typeArguments += typeArgumentVisitor
    typeArgumentVisitor.visitTypeArgument(wildcard)
  }
}

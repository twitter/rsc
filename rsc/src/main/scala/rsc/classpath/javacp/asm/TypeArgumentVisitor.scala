// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
// NOTE: This file has been partially copy/pasted from scalameta/scalameta.
package rsc.classpath.javacp.asm

import rsc.classpath.javacp._
import org.objectweb.asm.signature.SignatureVisitor

class TypeArgumentVisitor extends TypedSignatureVisitor[TypeArgument] {
  var wildcard = Option.empty[WildcardIndicator]
  val referenceTypeSignature = new ReferenceTypeSignatureVisitor
  override def result(): TypeArgument =
    wildcard match {
      case Some(WildcardIndicator.Star) => WildcardTypeArgument
      case _ => ReferenceTypeArgument(wildcard, referenceTypeSignature.result().get)
    }

  override def visitTypeArgument(wildcard: Char): SignatureVisitor = {
    this.wildcard = wildcard match {
      case '+' => Some(WildcardIndicator.Plus)
      case '-' => Some(WildcardIndicator.Minus)
      case _ => None
    }
    referenceTypeSignature
  }
  override def visitTypeArgument(): Unit = {
    this.wildcard = Some(WildcardIndicator.Star)
  }
}

// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
// NOTE: This file has been partially copy/pasted from scalameta/scalameta.
package rsc.classpath.javacp.asm

import rsc.classpath.javacp._
import org.objectweb.asm.signature.SignatureVisitor

class TypeParameterVisitor(identifier: String) extends TypedSignatureVisitor[TypeParameter] {
  val classBound = new ReferenceTypeSignatureVisitor
  val interfaceBounds = List.newBuilder[ReferenceTypeSignatureVisitor]
  override def result(): TypeParameter =
    TypeParameter(
      identifier,
      ClassBound(classBound.result()),
      interfaceBounds.result().map { ib =>
        InterfaceBound(ib.result().get)
      }
    )

  override def visitClassBound(): SignatureVisitor = {
    classBound
  }

  override def visitInterfaceBound(): SignatureVisitor = {
    val visitor = new ReferenceTypeSignatureVisitor
    interfaceBounds += visitor
    visitor
  }
}

// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
// NOTE: This file has been partially copy/pasted from scalameta/scalameta.
package rsc.classpath.javacp.asm

import rsc.classpath.javacp._
import org.objectweb.asm.signature.SignatureVisitor

class MethodSignatureVisitor
    extends TypedSignatureVisitor[MethodSignature]
    with TypeParametersVisitor {
  val params = List.newBuilder[JavaTypeSignatureVisitor]
  val returnType = new JavaTypeSignatureVisitor(false)
  val throws = List.newBuilder[ReferenceTypeSignatureVisitor]

  override def result(): MethodSignature = {
    val tparams = super.typeParametersResult()
    MethodSignature(
      tparams,
      params.result().map(_.result()),
      returnType.result(),
      throws.result().map(_.result().get).map {
        case cts: ClassTypeSignature => ThrowsSignature.ClassType(cts)
        case tvs: TypeVariableSignature => ThrowsSignature.TypeVariable(tvs)
        case els => throw new IllegalArgumentException(s"Expected ThrowsSignature, obtained $els")
      }
    )
  }

  override def visitParameterType: SignatureVisitor = {
    val visitor = new JavaTypeSignatureVisitor(false)
    params += visitor
    visitor
  }

  override def visitReturnType: SignatureVisitor = {
    returnType
  }

  override def visitExceptionType: SignatureVisitor = {
    val visitor = new ReferenceTypeSignatureVisitor
    throws += visitor
    visitor
  }
}

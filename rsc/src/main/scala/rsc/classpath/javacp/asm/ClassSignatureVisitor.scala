// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
// NOTE: This file has been partially copy/pasted from scalameta/scalameta.
package rsc.classpath.javacp.asm

import rsc.classpath.javacp._
import org.objectweb.asm.signature.SignatureVisitor

class ClassSignatureVisitor
    extends TypedSignatureVisitor[ClassSignature]
    with TypeParametersVisitor {
  private val superclassSignature = new ReferenceTypeSignatureVisitor
  private val superinterfaceSignatures = List.newBuilder[ReferenceTypeSignatureVisitor]

  override def result(): ClassSignature = {
    val tparams = super.typeParametersResult()
    val superclass = superclassSignature.classTypeSignature()
    val interfaces = superinterfaceSignatures.result().map(_.classTypeSignature())
    ClassSignature(tparams, superclass, interfaces)
  }

  override def visitSuperclass(): SignatureVisitor = {
    superclassSignature.visitSuperclass()
  }

  override def visitInterface(): SignatureVisitor = {
    val visitor = new ReferenceTypeSignatureVisitor
    superinterfaceSignatures += visitor
    visitor
  }

}

// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
// NOTE: This file has been partially copy/pasted from scalameta/scalameta.
package rsc.classpath.javacp.asm

import rsc.classpath.javacp._
import org.objectweb.asm.signature.SignatureVisitor

trait TypeParametersVisitor { this: SignatureVisitor =>
  private var typeParameters = List.newBuilder[TypeParameterVisitor]
  private var lastTypeParameterVisitor: TypeParameterVisitor = _

  def typeParametersResult(): Option[TypeParameters] = typeParameters.result() match {
    case Nil => None
    case head :: tail =>
      Some(TypeParameters(head.result(), tail.map(_.result())))
  }
  override def visitFormalTypeParameter(name: String): Unit = {
    val visitor = new TypeParameterVisitor(name)
    typeParameters += visitor
    lastTypeParameterVisitor = visitor
  }

  override def visitClassBound(): SignatureVisitor = {
    lastTypeParameterVisitor.visitClassBound()
  }

  override def visitInterfaceBound(): SignatureVisitor = {
    lastTypeParameterVisitor.visitInterfaceBound()
  }
}

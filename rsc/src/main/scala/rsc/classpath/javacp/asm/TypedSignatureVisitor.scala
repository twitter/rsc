// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
// NOTE: This file has been partially copy/pasted from scalameta/scalameta.
package rsc.classpath.javacp.asm

/** Helper to abstract over SignatureVisitors that produce a result of type T */
abstract class TypedSignatureVisitor[+T] extends FailFastSignatureVisitor {
  def result(): T
}

// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scala.meta.scalasig

import scala.meta.internal.scalasig._

case class Classfile(
    name: String,
    source: String,
    scalasigBytes: Option[Array[Byte]]) {
  def toBinary: Array[Byte] = {
    try {
      ClassfileCodec.toBinary(this)
    } catch {
      case ex: Throwable =>
        throw ClassfileWriteException(this, ex)
    }
  }
}

object Classfile {
  def fromBinary(binary: Binary): ClassfileResult = {
    try {
      val classfile = ClassfileCodec.fromBinary(binary)
      ParsedClassfile(binary, classfile)
    } catch {
      case ex: Throwable =>
        FailedClassfile(binary, ClassfileReadException(binary, ex))
    }
  }
}

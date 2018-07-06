// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scala.meta.scalasig

import scala.meta.internal.scalasig._
import scala.util.control.NonFatal

case class Classfile(name: String, scalasigBytes: Option[Array[Byte]]) {
  def toBinary: Array[Byte] = {
    try {
      ClassfileCodec.toBinary(this)
    } catch {
      case NonFatal(ex) =>
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
      case NonFatal(ex) =>
        FailedClassfile(binary, ClassfileReadException(binary, ex))
    }
  }
}

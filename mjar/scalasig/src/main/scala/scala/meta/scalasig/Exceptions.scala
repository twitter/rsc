// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scala.meta.scalasig

import scala.meta.scalasig.{highlevel => h}
import scala.meta.scalasig.{lowlevel => l}

case class ClassfileReadException(binary: Binary, cause: Throwable) extends Exception {
  override def getMessage: String = {
    val details = s"${cause.getClass.getName}: ${cause.getMessage}"
    "failed to read classfile from ${binary}: $details"
  }
  setStackTrace(cause.getStackTrace)
}

case class ClassfileWriteException(classfile: Classfile, cause: Throwable) extends Exception {
  override def getMessage: String = {
    val details = s"${cause.getClass.getName}: ${cause.getMessage}"
    s"failed to write classfile ${classfile.name}: $details"
  }
  setStackTrace(cause.getStackTrace)
}

case class ScalasigReadException(binary: Binary, classfile: Classfile, cause: Throwable)
    extends Exception {
  override def getMessage: String = {
    val where = binary match {
      case NoBinary => classfile.name + ".class"
      case other => other.toString
    }
    val details = s"${cause.getClass.getName}: ${cause.getMessage}"
    s"failed to read scalasig from $where: $details"
  }
  setStackTrace(cause.getStackTrace)
}

case class ScalasigWriteException(scalasig: l.Scalasig, cause: Throwable) extends Exception {
  override def getMessage: String = {
    val details = s"${cause.getClass.getName}: ${cause.getMessage}"
    s"failed to write scalasig: $details"
  }
  setStackTrace(cause.getStackTrace)
}

case class ScalasigConvertException(scalasig: Any, cause: Throwable) extends Exception {
  override def getMessage: String = {
    val name = scalasig match {
      case h.Scalasig(name, _, _) => name
      case l.Scalasig(name, _, _) => name
      case _ => "unknown"
    }
    val details = s"${cause.getClass.getName}: ${cause.getMessage}"
    s"failed to convert scalasig $name: $details"
  }
  setStackTrace(cause.getStackTrace)
}

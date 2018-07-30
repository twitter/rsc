// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scala.meta.scalasig.lowlevel

import scala.meta.internal.scalasig._
import scala.meta.scalasig._
import scala.meta.scalasig.{highlevel => h}

case class Scalasig(name: String, entries: Array[Entry]) extends Pretty {
  def toHighlevel: h.Scalasig = {
    try {
      ScalasigHighlevel(this)
    } catch {
      case ex: Throwable =>
        throw ScalasigConvertException(this, ex)
    }
  }

  def toClassfile: Classfile = {
    try {
      ScalasigCodec.toClassfile(this)
    } catch {
      case ex: Throwable =>
        throw ScalasigWriteException(this, ex)
    }
  }

  def toBinary: Array[Byte] = {
    this.toClassfile.toBinary
  }
}

object Scalasig {
  def fromBinary(binary: Binary): ScalasigResult = {
    Classfile.fromBinary(binary) match {
      case ParsedClassfile(_, classfile) =>
        try {
          ScalasigCodec.fromClassfile(classfile) match {
            case Some(scalasig) =>
              ParsedScalasig(binary, classfile, scalasig)
            case None =>
              EmptyScalasig(binary, classfile)
          }
        } catch {
          case ex: Throwable =>
            val cause = ScalasigReadException(binary, classfile, ex)
            FailedScalasig(binary, classfile, cause)
        }
      case result: FailedClassfile =>
        result
    }
  }

  def fromClassfile(classfile: Classfile): ScalasigResult = {
    try {
      ScalasigCodec.fromClassfile(classfile) match {
        case Some(scalasig) =>
          ParsedScalasig(NoBinary, classfile, scalasig)
        case None =>
          EmptyScalasig(NoBinary, classfile)
      }
    } catch {
      case ex: Throwable =>
        val cause = ScalasigReadException(NoBinary, classfile, ex)
        FailedScalasig(NoBinary, classfile, cause)
    }
  }
}

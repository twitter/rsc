// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scala.meta.scalasig.highlevel

import scala.meta.internal.scalasig._
import scala.meta.scalasig._
import scala.meta.scalasig.{lowlevel => l}
import scala.util.control.NonFatal

case class Scalasig(name: String, symbols: List[EmbeddedSymbol])
    extends Pretty {
  def toLowlevel: l.Scalasig = {
    try {
      ScalasigLowlevel(this)
    } catch {
      case NonFatal(ex) =>
        throw ScalasigConvertException(this, ex)
    }
  }

  def toClassfile: Classfile = {
    this.toLowlevel.toClassfile
  }

  def toBinary: Array[Byte] = {
    this.toLowlevel.toBinary
  }
}

object Scalasig {
  def fromBinary(binary: Binary): ScalasigResult = {
    l.Scalasig.fromBinary(binary) match {
      case l.ParsedScalasig(binary, classfile, lscalasig) =>
        try {
          val hscalasig = ScalasigHighlevel(lscalasig)
          ParsedScalasig(binary, classfile, hscalasig)
        } catch {
          case NonFatal(ex) =>
            val cause = ScalasigConvertException(lscalasig, ex)
            FailedScalasig(binary, classfile, cause)
        }
      case result: ScalasigResult =>
        result
    }
  }

  def fromClassfile(classfile: Classfile): ScalasigResult = {
    l.Scalasig.fromClassfile(classfile) match {
      case l.ParsedScalasig(binary, classfile, lscalasig) =>
        try {
          val hscalasig = ScalasigHighlevel(lscalasig)
          ParsedScalasig(binary, classfile, hscalasig)
        } catch {
          case NonFatal(ex) =>
            val cause = ScalasigConvertException(lscalasig, ex)
            FailedScalasig(binary, classfile, cause)
        }
      case result: ScalasigResult =>
        result
    }
  }
}

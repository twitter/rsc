// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scala.meta

import scala.meta.scalasig._
import scala.meta.scalasig.{highlevel => h}
import scala.meta.scalasig.{lowlevel => l}

package scalasig {
  sealed trait ClassfileResult {
    def binary: Binary
  }

  case class FailedClassfile(binary: Binary, cause: Throwable)
      extends ClassfileResult
      with l.ScalasigResult
      with h.ScalasigResult

  case class ParsedClassfile(binary: Binary, classfile: Classfile) extends ClassfileResult

  case class FailedScalasig(binary: Binary, classfile: Classfile, cause: Throwable)
      extends l.ScalasigResult
      with h.ScalasigResult

  case class EmptyScalasig(binary: Binary, classfile: Classfile)
      extends l.ScalasigResult
      with h.ScalasigResult
}

package scalasig.highlevel {
  sealed trait ScalasigResult {
    def binary: Binary
  }

  case class ParsedScalasig(binary: Binary, classfile: Classfile, scalasig: Scalasig)
      extends ScalasigResult
}

package scalasig.lowlevel {
  sealed trait ScalasigResult {
    def binary: Binary
  }

  case class ParsedScalasig(binary: Binary, classfile: Classfile, scalasig: Scalasig)
      extends ScalasigResult
}

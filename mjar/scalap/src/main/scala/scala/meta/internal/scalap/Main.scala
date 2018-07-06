// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scala.meta.internal.scalap

import scala.meta.cli._
import scala.meta.scalap._
import scala.meta.scalasig._
import scala.meta.scalasig.lowlevel._

class Main(settings: Settings, reporter: Reporter) {
  def process(): Boolean = {
    var first = true
    Scalasigs(settings.paths) { result =>
      result match {
        case ParsedScalasig(_, _, lowlevelScalasig) =>
          if (first) {
            first = false
          } else {
            reporter.out.println("")
          }
          settings.format match {
            case Format.Lowlevel =>
              reporter.out.println(lowlevelScalasig.toString)
            case Format.Highlevel =>
              try {
                val highlevelScalasig = lowlevelScalasig.toHighlevel
                reporter.out.println(highlevelScalasig.toString)
              } catch {
                case cause: ScalasigConvertException =>
                  cause.printStackTrace(reporter.out)
              }
          }
        case EmptyScalasig(_, _) =>
          ()
        case FailedScalasig(_, _, cause) =>
          cause.printStackTrace(reporter.out)
        case FailedClassfile(_, cause) =>
          cause.printStackTrace(reporter.out)
      }
    }
    true
  }
}

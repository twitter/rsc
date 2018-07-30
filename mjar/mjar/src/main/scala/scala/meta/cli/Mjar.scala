// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scala.meta.cli

import java.nio.file._
import scala.meta.internal.cli._
import scala.meta.internal.mjar._
import scala.meta.mjar._

object Mjar {
  def main(args: Array[String]): Unit = {
    val expandedArgs = Args.expand(args)
    val reporter = Reporter()
    Settings.parse(expandedArgs, reporter) match {
      case Some(settings) =>
        if (process(settings, reporter).nonEmpty) sys.exit(0)
        else sys.exit(1)
      case None =>
        sys.exit(1)
    }
  }

  def process(settings: Settings, reporter: Reporter): Option[Path] = {
    val main = new Main(settings, reporter)
    main.process()
  }
}

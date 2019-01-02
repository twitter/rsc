// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.cli

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file._
import rsc.Compiler
import rsc.pretty._
import rsc.report._
import rsc.settings._

object Main {
  def main(args: Array[String]): Unit = {
    val result = process(args)
    if (result) sys.exit(0) else sys.exit(1)
  }

  def process(args: Array[String]): Boolean = {
    val expandedArgs = {
      args.toList.flatMap { arg =>
        if (arg.startsWith("@")) {
          val argPath = Paths.get(arg.substring(1))
          val argText = new String(Files.readAllBytes(argPath), UTF_8)
          argText.split(EOL).map(_.trim).filter(_.nonEmpty).toList
        } else {
          List(arg)
        }
      }
    }
    Settings.parse(expandedArgs) match {
      case Some(settings) =>
        val reporter = ConsoleReporter(settings)
        val compiler = Compiler(settings, reporter)
        try {
          compiler.run()
          reporter.problems.isEmpty
        } finally {
          compiler.close()
        }
      case None =>
        false
    }
  }
}

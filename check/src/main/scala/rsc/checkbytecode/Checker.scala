// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.checkbytecode

import java.io._
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file._
import rsc.checkbase._
import scala.tools.jardiff._

class Checker(nscResult: Path, rscResult: Path) extends CheckerBase {
  def check(): Unit = {
    val baos = new ByteArrayOutputStream()
    val paths = List(List(nscResult), List(rscResult))
    val config = JarDiff.Config(
      gitRepo = None,
      code = true,
      raw = false,
      privates = true,
      contextLines = None,
      diffOutputStream = baos)
    val jarDiff = JarDiff(paths, config)
    val diffFound = jarDiff.diff()
    if (diffFound) {
      val output = new String(baos.toByteArray, UTF_8)
      problems += DifferentProblem(output)
    }
  }
}

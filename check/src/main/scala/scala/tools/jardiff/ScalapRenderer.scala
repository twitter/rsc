/*
 * Copyright (C) 2017 Lightbend Inc. <http://www.lightbenc.com>
 */
// NOTE: This file has been partially copy/pasted from scala/jardiff.

package scala.tools.jardiff

import java.nio.file.{Files, Path}

import scala.tools.scalap.scalax.rules.ScalaSigParserError

class ScalapRenderer() extends FileRenderer {
  def outFileExtension: String = ".scalap"
  override def render(in: Path, out: Path): Unit = {
    val classBytes = Files.readAllBytes(in)
    try {
      val decompiled = scala.tools.scalap.Main.decompileScala(classBytes, in.getFileName.toString == "package.class")
      if (decompiled != "") {
        Files.createDirectories(out.getParent)
        Files.write(out, decompiled.getBytes("UTF-8"))
      }
    } catch {
      case err: ScalaSigParserError =>
        System.err.println("WARN: unable to invoke scalap on: " + in + ": " + err.getMessage)
    }
  }
}

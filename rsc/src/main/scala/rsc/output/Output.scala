// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.output

import java.nio.file._
import rsc.settings._

sealed trait Output extends AutoCloseable {
  def write(path: Path, bytes: Array[Byte]): Unit
}

class DirectoryOutput(settings: Settings) extends Output {
  def write(path: Path, bytes: Array[Byte]): Unit = {
    val absolutePath = settings.d.resolve(path).toAbsolutePath
    Files.createDirectories(absolutePath.getParent)
    Files.write(absolutePath, bytes)
  }

  def close(): Unit = {
    ()
  }
}

object Output {
  def apply(settings: Settings): Output = {
    if (settings.d.toString.endsWith(".jar")) {
      ???
    } else {
      new DirectoryOutput(settings)
    }
  }
}

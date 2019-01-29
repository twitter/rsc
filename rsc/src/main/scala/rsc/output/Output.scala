// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.output

import java.io._
import java.nio.file._
import java.util.jar._
import java.util.zip.Deflater._
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

class JarOutput(settings: Settings) extends Output {
  private var jos: JarOutputStream = _

  private def ensureStream(): Unit = {
    if (jos != null) return
    Files.createDirectories(settings.d.toAbsolutePath.getParent)
    val os = Files.newOutputStream(settings.d)
    val bos = new BufferedOutputStream(os)
    jos = new JarOutputStream(bos)
    jos.setLevel(NO_COMPRESSION)
  }

  def write(path: Path, bytes: Array[Byte]): Unit = {
    ensureStream()
    jos.putNextEntry(new JarEntry(path.toString))
    jos.write(bytes)
    jos.closeEntry()
  }

  def close(): Unit = {
    if (jos == null) return
    jos.close()
  }
}

object Output {
  def apply(settings: Settings): Output = {
    if (settings.d.toString.endsWith(".jar")) {
      new JarOutput(settings)
    } else {
      new DirectoryOutput(settings)
    }
  }
}

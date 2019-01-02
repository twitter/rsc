// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.checkbase

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file._
import scala.meta.scalasig._

trait DumpUtil {
  implicit class StringDumpOps(s: String) {
    def dump(extension: String = ".txt"): Path = {
      val tmpPath = Files.createTempFile("", extension)
      Files.write(tmpPath, s.getBytes(UTF_8))
      tmpPath
    }
  }

  implicit class BytesDumpOps(bytes: Array[Byte]) {
    def dump(extension: String = ".bin"): Path = {
      val tmpPath = Files.createTempFile("", extension)
      Files.write(tmpPath, bytes)
      tmpPath
    }
  }

  implicit class ClassfileDumpOps(classfile: Classfile) {
    def dump(extension: String = ".class"): Path = {
      val tmpPath = Files.createTempFile("", extension)
      Files.write(tmpPath, classfile.toBinary)
      tmpPath
    }
  }
}

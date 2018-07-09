// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.tests

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file._
import scala.meta.scalasig._

trait DumpUtil {
  implicit class StringDumpOps(s: String) {
    def dump(): Path = {
      val tmpPath = Files.createTempFile("", ".txt")
      Files.write(tmpPath, s.getBytes(UTF_8))
      tmpPath
    }
  }

  implicit class BytesDumpOps(bytes: Array[Byte]) {
    def dump(): Path = {
      val tmpPath = Files.createTempFile("", ".bin")
      Files.write(tmpPath, bytes)
      tmpPath
    }
  }

  implicit class ClassfileDumpOps(classfile: Classfile) {
    def dump(): Path = {
      val tmpPath = Files.createTempFile("", ".class")
      Files.write(tmpPath, classfile.toBinary)
      tmpPath
    }
  }
}

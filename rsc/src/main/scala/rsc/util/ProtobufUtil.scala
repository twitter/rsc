// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.util

import java.io._
import java.nio.file._
import scalapb._

trait ProtobufUtil {
  implicit class MessageUtil(val m: GeneratedMessage) {
    def writeTo(path: Path): Unit = {
      Files.createDirectories(path.toAbsolutePath.getParent)
      val fos = Files.newOutputStream(path)
      val bos = new BufferedOutputStream(fos)
      try {
        m.writeTo(bos)
      } finally {
        bos.close()
        fos.close()
      }
    }
  }
}

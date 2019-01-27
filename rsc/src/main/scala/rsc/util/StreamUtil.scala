// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
// NOTE: This file has been partially copy/pasted from scalameta/scalameta.
package rsc.util

import java.io._

trait StreamUtil {
  implicit class InputStreamOps(is: InputStream) {
    def readAllBytes(): Array[Byte] = {
      val baos = new ByteArrayOutputStream()
      val buf = new Array[Byte](1024)
      var len = is.read(buf)
      while (len != -1) {
        baos.write(buf, 0, len)
        len = is.read(buf)
      }
      baos.toByteArray
    }
  }
}

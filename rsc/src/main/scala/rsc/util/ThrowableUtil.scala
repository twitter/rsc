// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.util

import java.io._

trait ThrowableUtil {
  implicit class ThrowableOps(ex: Throwable) {
    def str: String = {
      val sw = new StringWriter()
      ex.printStackTrace(new PrintWriter(sw))
      sw.toString.trim
    }
  }
}

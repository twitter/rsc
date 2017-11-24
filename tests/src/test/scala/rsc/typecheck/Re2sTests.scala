// Copyright (c) 2017 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.typecheck

import utest._
import rsc.tests._

object Re2sTests extends RscTests {
  val tests = Tests {
    "typecheck re2s" - {
      val compiler = mkCompiler("-Ystop-after:typecheck", re2sRscFiles)
      assertRun(compiler)
    }
  }
}

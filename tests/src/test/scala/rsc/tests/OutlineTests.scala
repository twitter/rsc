// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.tests

import rsc.checkoutline._

class OutlineTests extends RscTests {
  test("outline for core") {
    val settings = Settings(coreClasspath, coreFiles)
    val problems = Main.process(settings)
    if (problems.nonEmpty) fail()
  }
}

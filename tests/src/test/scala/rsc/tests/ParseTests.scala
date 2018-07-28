// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.tests

import rsc.checkparse._

class ParseTests extends RscTests {
  test("parse for core") {
    val settings = Settings(coreFiles, quiet = true)
    val problems = Main.process(settings)
    if (problems.nonEmpty) fail()
  }

  test("parse for external") {
    val settings = Settings(externalFiles, quiet = true)
    val problems = Main.process(settings)
    if (problems.nonEmpty) fail()
  }
}

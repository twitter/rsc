// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.tests

import rsc.checkmjar._
import scala.meta.cli._

class MjarTests extends RscTests {
  test("mjar for core") {
    val reporter = Reporter()
    val settings = Settings(coreClasspath, coreFiles, quiet = true)
    val problems = Main.process(reporter, settings)
    if (problems.nonEmpty) fail()
  }
}

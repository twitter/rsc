// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.tests

import rsc.checkoutline._
import scala.meta.cli._

class OutlineTests extends RscTests {
  test("outline for core") {
    val reporter = Reporter()
    val settings = Settings(coreClasspath, coreFiles, quiet = true)

    val problems = MainCommand.process(reporter, settings)
    if (problems.nonEmpty) fail()
  }

  test("outline for function") {
    val reporter = Reporter()
    val settings = Settings(functionClasspath, functionFiles, quiet = true)
    val problems = MainCommand.process(reporter, settings)
    if (problems.nonEmpty) fail()
  }

  test("outline for semantic") {
    val reporter = Reporter()
    val settings = Settings(semanticClasspath, semanticFiles, quiet = true)
    val problems = MainCommand.process(reporter, settings)
    if (problems.nonEmpty) fail()
  }
}

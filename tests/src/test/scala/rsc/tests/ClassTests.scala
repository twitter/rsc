// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.tests

import rsc.checkclasses._
import scala.meta.cli._

class ClassTests extends RscTests {
  test("classes for semantic") {
    val reporter = Reporter()
    val settings = Settings(semanticClasspath, dependenciesFiles, semanticFiles, quiet = true)
    val problems = Main.process(reporter, settings)
    if (problems.nonEmpty) fail()
  }
}

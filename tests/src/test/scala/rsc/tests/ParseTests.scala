// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.tests

import rsc.checkparse._
import scala.meta.cli._

class ParseTests extends RscTests {
  test("parse for core") {
    val reporter = Reporter()
    val settings = Settings(coreFiles, quiet = true)
    val problems = Main.process(reporter, settings)
    if (problems.nonEmpty) fail()
  }

  test("parse for function") {
    val reporter = Reporter()
    val settings = Settings(functionFiles, quiet = true)
    val problems = Main.process(reporter, settings)
    if (problems.nonEmpty) fail()
  }

  test("parse for syntactic") {
    val reporter = Reporter()
    val settings = Settings(syntacticFiles, quiet = true)
    val problems = Main.process(reporter, settings)
    if (problems.nonEmpty) fail()
  }
}

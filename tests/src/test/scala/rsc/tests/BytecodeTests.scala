// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.tests

import rsc.checkbytecode._
import scala.meta.cli._

class BytecodeTests extends RscTests {
  test("bytecode for semantic") {
    val reporter = Reporter()
    val settings = Settings(
      depsClasspath,
      depsFiles,
      semanticFiles,
      quiet = true
    )
    val problems = Main.process(reporter, settings)
    if (problems.nonEmpty) fail()
  }
}

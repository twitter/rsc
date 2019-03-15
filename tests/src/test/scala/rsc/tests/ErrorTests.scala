package rsc.tests

class ErrorTests extends RscTests {

  private val errorFilesMap = errorFiles.map(path => path.getFileName.toString -> path).toMap

  test("Method and field definition with No Type generates errors") {

    val filename = "408.scala"

    val expectedFailures = List(
      notype(filename, "4:2..4:12", "val def <ticket408/C#x.> = \"\""),
      notype(filename, "6:2..6:22", "protected def <ticket408/C#y().> = 42")
    )

    val failures = problemsWith(filename)

    checkFailures(failures, expectedFailures)
  }

  private def problemsWith(filename: String): List[String] = {
    rsc(errorClasspath, List(errorFilesMap(filename))).fold(problems => problems, _ => Nil)
  }

  private def checkFailures(failures: List[String], expectedFailures: List[String]): Unit = {

    if (failures.length != expectedFailures.length) {
      fail(
        s"""|
            |Expected ${expectedFailures.length} failures but only got ${failures.length}.
            |
            |Expected:
            |$expectedFailures
            |
            |Actual:
            |$failures
            |""".stripMargin
      )
    }

    failures.zip(expectedFailures).foreach {
      case (failure, expectedFailure) if !failure.contains(expectedFailure) =>
        fail(
          s"""|
              |Expected failure to contain:
              |
              |$expectedFailure
              |
              |but got:
              |
              |$failure
              |""".stripMargin
        )
      case _ =>
        ()
    }
  }

  private def notype(filename: String, position: String, line: String): String = {
    val absfile = errorFilesMap(filename).toAbsolutePath.toString

    s"error: No type found at $absfile@$position for definition: $line"
  }
}

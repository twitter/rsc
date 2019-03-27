package rsc.tests

class ErrorTests extends RscTests {

  test("Method and field definition with No Type generates errors") {

    val filename = "408.scala"

    val expectedFailures = List(
      notypeErrorMsg(filename, "4:2..4:12", "val def <ticket408/C#x.> = \"\""),
      notypeErrorMsg(filename, "6:2..6:22", "protected def <ticket408/C#y().> = 42"),
      notypeErrorMsg(filename, "12:2..12:13", "var def <ticket408/C#x3().> = \"\""),
      notypeErrorMsg(filename, "16:2..16:32", "private[<ticket408/>] def <ticket408/C#y3().> = 45")
    )

    val failures = problemsWith(filename)

    checkFailures(failures, expectedFailures)
  }

  test("Class definition with Init without required type parameters generates errors") {

    val filename = "410.scala"

    val expectedFailures = List(
      initNoTypeParamErrorMsg(filename, "8:16..8:20", "<ticket410/C#>(1)"),
      initNoTypeParamErrorMsg(filename, "10:16..10:17", "<ticket410/C#>"),
      initNoTypeParamErrorMsg(filename, "12:25..12:29", "<ticket410/C#>(2)"),
      initNoTypeParamErrorMsg(filename, "14:26..14:30", "<ticket410/C#>(3)"),
      initNoTypeParamErrorMsg(filename, "16:27..16:31", "<ticket410/C#>(x)"),
      initNoTypeParamErrorMsg(filename, "18:37..18:41", "<ticket410/C#>(x)"),
      initNoTypeParamErrorMsg(filename, "20:27..20:31", "<ticket410/C#>(4)"),
      initNoTypeParamErrorMsg(filename, "26:17..26:22", "<ticket410/CB#>(6)"),
      initNoTypeParamErrorMsg(filename, "28:26..28:31", "<ticket410/CB#>(7)"),
      initNoTypeParamErrorMsg(filename, "30:27..30:32", "<ticket410/CB#>(8)")
    )

    val failures = problemsWith(filename)

    checkFailures(failures, expectedFailures)
  }

  ////////
  // HELPERS
  ////////

  private val errorFilesMap =
    errorFiles.last.map(path => path.getFileName.toString -> path).toMap

  private def problemsWith(filename: String): List[String] = {
    rscs(errorClasspath, errorFiles.init :+ List(errorFilesMap(filename)))
      .fold( // Either.fold
        problems => problems,
        _ => Nil // success
      )
  }

  private def checkFailures(failures: List[String], expectedFailures: List[String]): Unit = {

    if (failures.length != expectedFailures.length) {
      fail(
        s"""|
            |Expected ${expectedFailures.length} failures but got ${failures.length}.
            |
            |Expected:
            |${listErrors(expectedFailures)}
            |
            |Actual:
            |${listErrors(failures)}
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

  private def listErrors(messages: List[String]): String = {
    messages.zipWithIndex.foldLeft("") {
      case (acc, (msg, idx)) =>
        val s =
          s"""|
              |$idx:
              |$msg
              |===========""".stripMargin
        acc ++ s
    }
  }

  private def absfile(filename: String): String = errorFilesMap(filename).toAbsolutePath.toString

  private def notypeErrorMsg(filename: String, position: String, defn: String): String =
    s"error: No type found at ${absfile(filename)}@$position for definition: $defn"

  private def initNoTypeParamErrorMsg(filename: String, position: String, init: String): String =
    s"error: Type parameters required but missing at ${absfile(filename)}@$position for parent init: $init"
}

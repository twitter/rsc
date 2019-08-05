package rsc.tests
import java.nio.file.Paths
import org.scalatest.FunSuite
import rsc.checkbase.SourceDependencies

class CheckBaseTests extends FunSuite {
  test("parses source dependencies from cli arg") {
    val input = "A.scala:B.scala,C.scala:D.scala"
    val result = SourceDependencies
    /**EndMarker*/.parsePaths(input).parsePaths(input)
    val expected = List(
      List(Paths.get("A.scala"), Paths.get("B.scala")),
      List(Paths.get("C.scala"), Paths.get("D.scala"))
    )
    assert(result == expected)
  }
}

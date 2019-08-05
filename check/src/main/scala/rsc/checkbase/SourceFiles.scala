package rsc.checkbase
import cats.data.{NonEmptyList, Validated}
import com.monovore.decline.Argument
import java.nio.file.{Files, Path, Paths}

case class SourceFiles(sources: List[Path])

object SourceFiles {
  implicit val configArgument: Argument[SourceFiles] = new Argument[SourceFiles] {

    def read(string: String): Validated[NonEmptyList[String], SourceFiles] = {
      val paths = string.split(',').map(str => Paths.get(str)).toList
      val unreadablePaths =
        paths.filterNot(path => Files.isReadable(path) && Files.isRegularFile(path))
      if (string.isEmpty) Validated.invalidNel("No sources were provided")
      else if (unreadablePaths.nonEmpty || string.isEmpty) {
        val errs =
          Validated.invalid(
            NonEmptyList(unreadablePaths.head, unreadablePaths.tail)
              .map(unread => s"$unread is not a readable file"))
        errs
      } else Validated.valid(SourceFiles(paths.toList))
    }

    def defaultMetavar = "sources"
  }
}

package rsc.checkbase

import cats.data._
import com.monovore.decline._
import java.io.File.pathSeparator
import java.nio.file.{Files, Path, Paths}

case class CompileClasspath(paths: List[Path])

object CompileClasspath {
  implicit val configArgument: Argument[CompileClasspath] = new Argument[CompileClasspath] {

    def read(string: String): Validated[NonEmptyList[String], CompileClasspath] = {
      val paths = string.split(pathSeparator).map(str => Paths.get(str)).toList
      val unreadablePaths = paths.filterNot(Files.isReadable)
      if (string.isEmpty) Validated.invalidNel("No classpath was provided")
      else if (unreadablePaths.nonEmpty || string.isEmpty) {
        val errs =
          Validated.invalid(NonEmptyList(unreadablePaths.head, unreadablePaths.tail).map(unread =>
            s"$unread is not readable"))
        errs
      } else Validated.valid(CompileClasspath(paths.toList))
    }

    val defaultMetavar: String = "classpath"
  }
}

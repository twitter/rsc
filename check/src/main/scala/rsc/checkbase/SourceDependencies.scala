package rsc.checkbase

import cats.data._
import com.monovore.decline._
import java.io.File.pathSeparator
import java.nio.file.{Files, Path, Paths}

final case class SourceDependencies(paths: List[List[Path]])

object SourceDependencies {
  implicit val configArgument: Argument[SourceDependencies] = new Argument[SourceDependencies] {

    def read(string: String): Validated[NonEmptyList[String], SourceDependencies] = {
      val paths = parsePaths(string)
      val unreadablePaths = paths.flatten.filterNot(Files.isReadable)
      if (string.isEmpty) Validated.invalidNel("No classpath was provided")
      else if (unreadablePaths.nonEmpty || string.isEmpty) {
        val errs =
          Validated.invalid(NonEmptyList(unreadablePaths.head, unreadablePaths.tail).map(unread =>
            s"$unread is not readable"))
        errs
      } else Validated.valid(SourceDependencies(paths))
    }

    val defaultMetavar: String = "source dependencies"
  }

  def parsePaths(arg: String): List[List[Path]] = {
    arg
      .split(",")
      .map(str => str.split(pathSeparator).toList)
      .map(str => str.map(str => Paths.get(str)))
      .toList
  }
}

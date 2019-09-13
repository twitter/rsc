// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.checkbase

import java.io.File.pathSeparator
import java.nio.file.{Path, Paths}

trait SettingsBase {
  def quiet: Boolean
}
object SettingsBase {

  final case class ClassfilesPath(rsc: Option[List[Path]], nsc: Option[List[Path]])

  def pathsFor(pathStr: String): List[Path] =
    pathStr.split(pathSeparator).map(s => Paths.get(s)).toList
}

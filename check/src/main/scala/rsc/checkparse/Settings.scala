// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.checkparse

import java.nio.file._

final case class Settings(ins: List[Path] = Nil)

object Settings {
  def parse(args: List[String]): Either[List[String], Settings] = {
    Right(Settings(args.map(s => Paths.get(s))))
  }
}

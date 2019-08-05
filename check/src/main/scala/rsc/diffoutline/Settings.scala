// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.diffoutline

import java.nio.file._
import rsc.checkbase._

final case class Settings(nscClasspath: List[Path], rscClasspath: List[Path], quiet: Boolean)
    extends SettingsBase

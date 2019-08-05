// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.checkoutline

import java.nio.file._
import rsc.checkbase._

final case class Settings(cp: List[Path] = Nil, ins: List[Path] = Nil, quiet: Boolean = false)
    extends SettingsBase

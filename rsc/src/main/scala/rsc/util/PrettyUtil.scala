// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.util

import rsc.pretty._
import rsc.settings._

trait PrettyUtil {
  implicit class PrinterUtilOps(p: Printer) {
    def header(value: String): Unit = {
      p.str(value)
      p.newline()
      p.str("=" * value.length)
      p.newline()
    }

    def settings: Settings = {
      val maybeSettings = p.props.get("settings").map(_.asInstanceOf[Settings])
      maybeSettings.getOrElse(Settings())
    }

    def settings_=(settings: Settings): Unit = {
      p.props("settings") = settings
    }
  }
}

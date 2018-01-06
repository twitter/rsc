// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.version

import rsc.internal.BuildInfo

trait Versions {
  private def ensureVersion(key: String, value: String): Version = {
    def fail = {
      val details = s"$key $value is not a valid version"
      sys.error(s"fatal error reading BuildInfo: $details")
    }
    Version.parse(value).getOrElse(fail)
  }

  lazy val current: Version = {
    ensureVersion("version", BuildInfo.version)
  }
}

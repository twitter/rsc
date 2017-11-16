// Copyright (c) 2017 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.version

import rsc.pretty._

// Stable builds are versioned using semver, e.g. 0.0.0.
// Backwards incompatible changes increment the major version (the first number).
// Other changes increment the minor version (the second number).
// Patch version (the third number) always stays at zero barring exceptional occasions.
//
// Prerelease builds are versioned using semver too, e.g. 0.0.0-45-b6e61074,
// where snapshot metadata (the number after the dash) is the distance from the previous release
// concatenated with the SHA of the current commit.
// If the working copy is dirty, we additionally append the timestamp to snapshot metadata,
// e.g. 0.0.0-45-b6e61074-20171004-1333

final case class Version(
    major: Int,
    minor: Int,
    patch: Int,
    snapshot: String,
    build: String
) extends Pretty {
  def printStr(p: Printer): Unit = PrettyVersion.str(p, this)
  def printRepl(p: Printer): Unit = PrettyVersion.repl(p, this)
}

object Version {
  def parse(s: String): Option[Version] = {
    val rxVersion = {
      """^(\d+)\.(\d+)\.(\d+)(?:-([0-9A-Za-z-\.]+)(?:\+([0-9A-Za-z-\.]+))?)?$""".r
    }
    s match {
      case rxVersion(s_major, s_minor, s_patch, s_snapshot, s_build) =>
        val major = s_major.toInt
        val minor = s_minor.toInt
        val patch = s_patch.toInt
        val snapshot = if (s_snapshot != null) s_snapshot else ""
        val build = if (s_build != null) s_build else ""
        Some(Version(major, minor, patch, snapshot, build))
      case _ =>
        None
    }
  }
}

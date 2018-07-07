// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.tests

import io.github.soc.directories.ProjectDirectories
import java.nio.file._

trait CacheUtil {
  def cacheDir(name: String, fingerprint: Fingerprint): Path = {
    val project = ProjectDirectories.from("com.twitter", "", name)
    val cacheRoot = project.cacheDir
    val cacheDir = Paths.get(cacheRoot).resolve(fingerprint.toString)
    Files.createDirectories(cacheDir)
    cacheDir
  }
}

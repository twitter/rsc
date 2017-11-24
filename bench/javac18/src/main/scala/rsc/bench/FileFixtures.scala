// Copyright (c) 2017 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.bench

import java.io._
import java.nio.file.Files
import scala.collection.JavaConverters._

trait FileFixtures {
  lazy val buildRoot: File = {
    BuildInfo.sourceRoot
  }

  lazy val re2jDir: File = {
    new File(s"$buildRoot/examples/re2j/src/main/java/java/util/regex")
  }

  lazy val re2jFiles: List[File] = {
    val stream = Files.newDirectoryStream(re2jDir.toPath)
    stream.asScala.map(_.toFile).toList
  }
}

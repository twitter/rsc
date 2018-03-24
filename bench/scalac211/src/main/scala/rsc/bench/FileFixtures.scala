// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.bench

import java.nio.file._
import scala.collection.JavaConverters._

trait FileFixtures {
  lazy val buildRoot: Path = {
    BuildInfo.sourceRoot.toPath
  }

  lazy val re2sDir: Path = {
    buildRoot.resolve("examples/re2s/src/main/scala/java/util/regex")
  }

  lazy val re2sScalacFiles: List[Path] = {
    val stream = Files.newDirectoryStream(re2sDir.toPath)
    stream.asScala.toList
  }
}

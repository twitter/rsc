// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.bench

import java.nio.file._
import scala.collection.JavaConverters._

trait FileFixtures {
  lazy val buildRoot: Path = {
    BuildInfo.sourceRoot.toPath
  }

  lazy val re2jDir: Path = {
    buildRoot.resolve("examples/re2j/src/main/java/com/google/re2j")
  }

  lazy val re2jFiles: List[Path] = {
    val stream = Files.newDirectoryStream(re2jDir)
    stream.asScala.toList
  }

  lazy val re2sDir: Path = {
    buildRoot.resolve("examples/re2s/src/main/scala/com/twitter/re2s")
  }

  lazy val re2sScalacFiles: List[Path] = {
    val stream = Files.newDirectoryStream(re2sDir)
    stream.asScala.toList
  }

  lazy val re2sRscFiles: List[Path] = {
    re2sScalacFiles :+ stdlibSourcepath
  }

  lazy val stdlibSourcepath: Path = {
    buildRoot.resolve("stdlib/src/main/scala/Stdlib.scala")
  }
}

// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.checkbase

import com.github.difflib.{DiffUtils, UnifiedDiffUtils}
import java.util
import rsc.pretty._
import scala.collection.JavaConverters._

trait DiffUtil {
  def diff(
      actualTitle: String,
      actualContent: String,
      expectTitle: String,
      expectContent: String): Option[String] = {
    val actualLines = actualContent.replaceAll("\\s+$", "").split(EOL).toList.asJava
    val expectLines = expectContent.replaceAll("\\s+$", "").split(EOL).toList.asJava

    // Diff algorithm will use random lookup, so we need an array.
    val actualLinesArray = new util.ArrayList[String](actualLines)
    val expectLinesArray = new util.ArrayList[String](expectLines)

    val diff = DiffUtils.diff(actualLinesArray, expectLinesArray)
    if (!diff.getDeltas.isEmpty) {
      val prettyDiff = UnifiedDiffUtils
        .generateUnifiedDiff(
          actualTitle,
          expectTitle,
          actualLinesArray,
          diff,
          5
        )
        .asScala
        .drop(3)
        .mkString(EOL)
      Some(prettyDiff)
    } else {
      None
    }
  }
}

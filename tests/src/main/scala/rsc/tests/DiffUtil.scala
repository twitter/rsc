// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.tests

import difflib.DiffUtils
import rsc.pretty._
import scala.collection.JavaConverters._

trait DiffUtil {
  def diff(
      actualTitle: String,
      actualContent: String,
      expectTitle: String,
      expectContent: String): Option[String] = {
    val actualLines = actualContent.trim.split(EOL).toList.asJava
    val expectLines = expectContent.trim.split(EOL).toList.asJava
    val diff = DiffUtils.diff(actualLines, expectLines)
    if (!diff.getDeltas.isEmpty) {
      val prettyDiff = DiffUtils
        .generateUnifiedDiff(
          actualTitle,
          expectTitle,
          actualLines,
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

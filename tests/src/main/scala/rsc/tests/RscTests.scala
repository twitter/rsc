// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.tests

import org.scalatest._
import rsc.checkbase._
import rsc.Compiler
import rsc.pretty._
import rsc.report._
import rsc.settings._

trait RscTests extends FunSuite with FileFixtures with DiffUtil with ToolUtil {
  def assertEquals(actual: String, expect: String): Unit = {
    assertEquals("actual", actual, "expect", expect)
  }

  def assertEquals(h1: String, s1: String, h2: String, s2: String): Unit = {
    val s1x = s1.split("\n").map(_.trim).mkString("\n")
    val s2x = s2.split("\n").map(_.trim).mkString("\n")
    if (s1x != s2x) {
      val problem = s"different $h1 (-) vs $h2 (+)"
      val compare = s"compare ${s1x.dump()} ${s2x.dump()}"
      val diff = this.diff(h1, s1x, h2, s2x).get
      fail(s"$problem: $compare$EOL$diff")
    }
  }

  def assertEquals(h1: String, b1: Array[Byte], h2: String, b2: Array[Byte]): Unit = {
    val xxd1 = xxd(b1).right.get
    val xxd2 = xxd(b2).right.get
    if (xxd1 != xxd2) {
      val problem = s"different $h1 (-) vs $h2 (+)"
      val compare = s"compare ${b1.dump()} ${b2.dump()}"
      val diff = this.diff(h1, xxd1, h2, xxd2).get
      fail(s"$problem: $compare$EOL$diff")
    }
  }

  def mkCompiler(args: Any*): Compiler = {
    val options = args.flatMap {
      case seq: Seq[_] => seq.map(_.toString)
      case other => List(other.toString)
    }
    val settings = Settings.parse(options.toList).get
    val reporter = StoreReporter(settings)
    Compiler(settings, reporter)
  }
}

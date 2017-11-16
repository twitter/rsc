// Copyright (c) 2017 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse

import java.nio.file._
import utest._
import rsc.tests._

object Re2sTests extends RscTests {
  val tests = Tests {
    "parse re2s" - {
      val files1 = re2sFiles
      val compiler1 = mkCompiler("-Ystop-after:parse", files1)
      assertRun(compiler1)
      val trees1 = compiler1.trees

      val files2 = trees1.map { tree =>
        val prefix = "rsc_"
        val suffix = "_" + tree.pos.input.file.getName
        val path = Files.createTempFile(prefix, suffix)
        Files.write(path, tree.str.getBytes)
        path.toFile
      }
      val compiler2 = mkCompiler("-Ystop-after:parse", files2)
      assertRun(compiler2)
      val trees2 = compiler2.trees

      trees1.zip(trees2).foreach {
        case (tree1, tree2) =>
          assertStructure(tree1, tree2)
      }
    }
  }
}

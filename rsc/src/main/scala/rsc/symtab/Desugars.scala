// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.symtab

import java.util.HashMap
import rsc.syntax._
import rsc.util._

trait Desugars {
  private val paramssDesugars = new HashMap[Parameterized, List[List[Param]]]
  private val parentsDesugars = new HashMap[DefnTemplate, List[Tpt]]
  private val returnsDesugars = new HashMap[Outline, Tpt]

  object desugars {
    object paramss {
      def apply(outline: Parameterized): List[List[Param]] = {
        val paramss = paramssDesugars.get(outline)
        if (paramss == null) crash(outline)
        paramss
      }

      def contains(outline: Parameterized): Boolean = {
        paramssDesugars.containsKey(outline)
      }

      def put(outline: Parameterized, paramss: List[List[Param]]): Unit = {
        if (paramssDesugars.containsKey(outline)) {
          crash(outline)
        }
        paramssDesugars.put(outline, paramss)
      }
    }

    object parents {
      def apply(outline: DefnTemplate): List[Tpt] = {
        val parents = parentsDesugars.get(outline)
        if (parents == null) crash(outline)
        parents
      }

      def put(outline: DefnTemplate, parents: List[Tpt]): Unit = {
        if (parentsDesugars.containsKey(outline)) {
          crash(outline)
        }
        parentsDesugars.put(outline, parents)
      }
    }

    object rets {
      def apply(outline: DefnDef): Tpt = {
        val ret = returnsDesugars.get(outline)
        if (ret == null) crash(outline)
        ret
      }

      def apply(outline: Self): Tpt = {
        val ret = returnsDesugars.get(outline)
        if (ret == null) crash(outline)
        ret
      }

      def contains(outline: DefnDef): Boolean = {
        returnsDesugars.containsKey(outline)
      }

      def contains(outline: Self): Boolean = {
        returnsDesugars.containsKey(outline)
      }

      def put(outline: DefnDef, ret: Tpt): Unit = {
        if (outline.ret.nonEmpty || returnsDesugars.containsKey(outline)) {
          crash(outline)
        }
        returnsDesugars.put(outline, ret)
      }

      def put(outline: Self, tpt: Tpt): Unit = {
        if (returnsDesugars.containsKey(outline)) {
          crash(outline)
        }
        returnsDesugars.put(outline, tpt)
      }
    }
  }
}

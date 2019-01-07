// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.symtab

import java.util.HashMap
import rsc.syntax._
import rsc.util._

trait Desugars {
  private val _paramssDesugars = new HashMap[Parameterized, List[List[Param]]]
  private val _parentsDesugars = new HashMap[DefnTemplate, List[Tpt]]
  private val _returnsDesugars = new HashMap[Outline, Tpt]

  object desugars {
    object paramss {
      def apply(outline: Parameterized): List[List[Param]] = {
        val paramss = _paramssDesugars.get(outline)
        if (paramss == null) crash(outline)
        paramss
      }

      def contains(outline: Parameterized): Boolean = {
        _paramssDesugars.containsKey(outline)
      }

      def put(outline: Parameterized, paramss: List[List[Param]]): Unit = {
        if (_paramssDesugars.containsKey(outline)) {
          crash(outline)
        }
        _paramssDesugars.put(outline, paramss)
      }
    }

    object parents {
      def apply(outline: DefnTemplate): List[Tpt] = {
        val parents = _parentsDesugars.get(outline)
        if (parents == null) crash(outline)
        parents
      }

      def put(outline: DefnTemplate, parents: List[Tpt]): Unit = {
        if (_parentsDesugars.containsKey(outline)) {
          crash(outline)
        }
        _parentsDesugars.put(outline, parents)
      }
    }

    object rets {
      def apply(outline: DefnDef): Tpt = {
        val ret = _returnsDesugars.get(outline)
        if (ret == null) crash(outline)
        ret
      }

      def apply(outline: Self): Tpt = {
        val ret = _returnsDesugars.get(outline)
        if (ret == null) crash(outline)
        ret
      }

      def contains(outline: DefnDef): Boolean = {
        _returnsDesugars.containsKey(outline)
      }

      def contains(outline: Self): Boolean = {
        _returnsDesugars.containsKey(outline)
      }

      def put(outline: DefnDef, ret: Tpt): Unit = {
        if (outline.ret.nonEmpty || _returnsDesugars.containsKey(outline)) {
          crash(outline)
        }
        _returnsDesugars.put(outline, ret)
      }

      def put(outline: Self, tpt: Tpt): Unit = {
        if (outline.tpt.nonEmpty || _returnsDesugars.containsKey(outline)) {
          crash(outline)
        }
        _returnsDesugars.put(outline, tpt)
      }
    }
  }
}

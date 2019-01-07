// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.symtab

import java.util.HashMap
import rsc.syntax._
import rsc.util._

trait Desugars {
  private val _paramss = new HashMap[Parameterized, List[List[Param]]]
  private val _parents = new HashMap[DefnTemplate, List[Tpt]]
  private val _returns = new HashMap[Outline, Tpt]

  object desugars {
    object paramss {
      def apply(outline: Parameterized): List[List[Param]] = {
        val paramss = _paramss.get(outline)
        if (paramss == null) crash(outline)
        paramss
      }

      def contains(outline: Parameterized): Boolean = {
        _paramss.containsKey(outline)
      }

      def put(outline: Parameterized, paramss: List[List[Param]]): Unit = {
        if (_paramss.containsKey(outline)) {
          crash(outline)
        }
        _paramss.put(outline, paramss)
      }
    }

    object parents {
      def apply(outline: DefnTemplate): List[Tpt] = {
        val parents = _parents.get(outline)
        if (parents == null) crash(outline)
        parents
      }

      def put(outline: DefnTemplate, parents: List[Tpt]): Unit = {
        if (_parents.containsKey(outline)) {
          crash(outline)
        }
        _parents.put(outline, parents)
      }
    }

    object rets {
      def apply(outline: DefnDef): Tpt = {
        val ret = _returns.get(outline)
        if (ret == null) crash(outline)
        ret
      }

      def apply(outline: Self): Tpt = {
        val ret = _returns.get(outline)
        if (ret == null) crash(outline)
        ret
      }

      def contains(outline: DefnDef): Boolean = {
        _returns.containsKey(outline)
      }

      def contains(outline: Self): Boolean = {
        _returns.containsKey(outline)
      }

      def put(outline: DefnDef, ret: Tpt): Unit = {
        if (outline.ret.nonEmpty || _returns.containsKey(outline)) {
          crash(outline)
        }
        _returns.put(outline, ret)
      }

      def put(outline: Self, tpt: Tpt): Unit = {
        if (outline.tpt.nonEmpty || _returns.containsKey(outline)) {
          crash(outline)
        }
        _returns.put(outline, tpt)
      }
    }
  }
}

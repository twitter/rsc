// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.semanticdb

import rsc.semantics._
import rsc.syntax._
import rsc.util._
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.Scala.{Descriptor => d}

trait Prefixes {
  self: Converter =>

  def prefix(id: Id): s.Type = {
    // FIXME: https://github.com/twitter/rsc/issues/198
    s.NoType
  }

  // NOTE: In this method, qual can either be TermPath (for Scala) or AmbigPath (for Java).
  // I briefly considered introducing QualPath that would be a common supertype for these traits,
  // but that sounded overly specific and was incompatible with existing tree fields called qual.
  def prefix(qual: Path, id: Id): s.Type = {
    val needsPrefix = {
      if (id.sym.owner != qual.id.sym) {
        id.sym.owner.desc match {
          case d.Term("package") => id.sym.owner.owner != qual.id.sym
          case _ => true
        }
      } else {
        false
      }
    }
    if (needsPrefix) {
      qual match {
        case id: AmbigId => s.TypeRef(prefix(id), id.sym, Nil)
        case AmbigSelect(qual, id) => s.TypeRef(prefix(qual, id), id.sym, Nil)
        case qual: TermPath => TptSingleton(qual).tpe
        case other => crash(other)
      }
    } else {
      s.NoType
    }
  }
}

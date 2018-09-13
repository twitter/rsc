// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.util

import rsc.inputs._
import rsc.syntax._

trait TreeUtil {
  implicit class TreeUtilIdOps(id: Id) {
    def opt: Option[Id] = {
      id match {
        case AnonId() => None
        case id => Some(id)
      }
    }
    def valueopt: Option[String] = {
      id match {
        case AmbigId(value) => Some(value)
        case AnonId() => None
        case NamedId(value) => Some(value)
      }
    }
    def isSymbolic: Boolean = {
      id match {
        case id: NamedId if id.value.nonEmpty =>
          id.value.last == '_' || isSymbolicIdPart(id.value.last)
        case _ =>
          false
      }
    }
  }

  implicit class TreeUtilModsOps(mods: Mods) {
    def :+(mod: Mod): Mods = {
      if (mod.pos != NoPosition) {
        val pos1 = Position(mods.pos.input, mods.pos.start, mod.pos.end)
        val mods1 = Mods(mods.trees :+ mod)
        mods1.withPos(pos1)
      } else {
        Mods(mods.trees :+ mod)
      }
    }

    def +:(mod: Mod): Mods = {
      if (mod.pos != NoPosition) {
        val pos1 = Position(mods.pos.input, mod.pos.start, mods.pos.end)
        val mods1 = Mods(mod +: mods.trees)
        mods1.withPos(pos1)
      } else {
        Mods(mod +: mods.trees)
      }
    }

    def filter(fn: Mod => Boolean): Mods = {
      Mods(mods.trees.filter(fn))
    }

    def map(fn: Mod => Mod): Mods = {
      Mods(mods.trees.map(fn))
    }

    def flatMap(fn: Mod => Iterable[Mod]): Mods = {
      Mods(mods.trees.flatMap(fn))
    }
  }
}

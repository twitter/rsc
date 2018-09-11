// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse.java

import rsc.lexis.java._
import rsc.syntax._

trait Imports {
  self: Parser =>

  def `import`(): Import = {
    val start = in.offset
    accept(IMPORT)
    val mods = this.mods()
    def loop(path: AmbigPath): AmbigSelect = {
      path match {
        case path: AmbigId =>
          accept(DOT)
          val id = ambigId()
          loop(atPos(path.pos.start)(AmbigSelect(path, id)))
        case path: AmbigSelect =>
          if (in.token == DOT) {
            in.nextToken()
            if (in.token == ASTERISK) {
              path
            } else {
              val id = ambigId()
              loop(atPos(path.pos.start)(AmbigSelect(path, id)))
            }
          } else {
            path
          }
      }
    }
    val path = loop(ambigId())
    val importer = {
      if (in.token == ASTERISK) {
        val start = in.offset
        in.nextToken()
        val importee = atPos(start)(ImporteeWildcard())
        atPos(mods.pos.start)(Importer(mods, path, List(importee)))
      } else {
        val AmbigSelect(qual, id) = path
        val importee = atPos(id.pos)(ImporteeName(id))
        atPos(mods.pos.start)(Importer(mods, qual, List(importee)))
      }
    }
    accept(SEMI)
    atPos(start)(Import(List(importer)))
  }
}

// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse.scala

import rsc.lexis.scala._
import rsc.syntax._

trait Imports {
  self: Parser =>

  def `import`(): Import = {
    val start = in.offset
    accept(IMPORT)
    val importers = this.importers()
    atPos(start)(Import(importers))
  }

  private def importers(): List[Importer] = {
    commaSeparated(importer)
  }

  private def importer(): Importer = {
    def loop(mods: Mods, qual: TermPath): Importer = {
      val start = qual.pos.start
      if (in.token == ID) {
        val tree = ambigId()
        if (in.token == DOT) {
          in.nextToken()
          val id = atPos(tree.pos)(TermId(tree.value))
          val qual1 = atPos(start)(TermSelect(qual, id))
          loop(mods, qual1)
        } else {
          val importees = List(atPos(tree.pos)(ImporteeName(tree)))
          atPos(start)(Importer(mods, qual, importees))
        }
      } else if (in.token == USCORE) {
        val importees = List(importee())
        atPos(start)(Importer(mods, qual, importees))
      } else if (in.token == LBRACE) {
        val importees = this.importees()
        atPos(start)(Importer(mods, qual, importees))
      } else {
        val errImportee = List(importee())
        atPos(start)(Importer(mods, qual, errImportee))
      }
    }
    val start = in.offset
    val mods = atPos(start)(Mods(Nil))
    val qual = termId()
    accept(DOT)
    loop(mods, qual)
  }

  private def importees(): List[Importee] = {
    if (in.token == RBRACE) Nil
    else inBraces(commaSeparated(importee))
  }

  private def importee(): Importee = {
    val start = in.offset
    if (in.token == ID) {
      val id1 = ambigId()
      if (in.token != ARROW) {
        atPos(id1.pos)(ImporteeName(id1))
      } else {
        val start = id1.pos.start
        in.nextToken()
        if (in.token == ID) {
          val id2 = ambigId()
          atPos(start)(ImporteeRename(id1, id2))
        } else if (in.token == USCORE) {
          in.nextToken()
          atPos(start)(ImporteeUnimport(id1))
        } else {
          val idErr = errorAmbigId()
          atPos(start)(ImporteeRename(id1, idErr))
        }
      }
    } else if (in.token == USCORE) {
      val start = in.offset
      in.nextToken()
      atPos(start)(ImporteeWildcard())
    } else {
      val idErr = errorAmbigId()
      atPos(start)(ImporteeName(idErr))
    }
  }
}

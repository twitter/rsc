// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse.java

import rsc.lexis.java._
import rsc.report._
import rsc.syntax._

trait Sources {
  self: Parser =>

  def source(): Source = {
    val start = in.offset
    val stats = {
      if (in.token == PACKAGE) {
        val mods = atPos(start)(Mods(Nil))
        in.nextToken()
        val id = packageId()
        accept(SEMI)
        val stats = packageStats()
        List(atPos(start)(DefnPackage(mods, id, stats)))
      } else {
        packageStats()
      }
    }
    atPos(start)(Source(stats))
  }

  private def packageId(): TermPath = {
    val start = in.offset
    def loop(path: TermPath): TermPath = {
      if (in.token == DOT) {
        accept(DOT)
        val id = termId()
        loop(atPos(start)(TermSelect(path, id)))
      } else {
        path
      }
    }
    loop(termId())
  }

  private def packageStats(): List[Stat] = {
    val stats = List.newBuilder[Stat]
    while (in.token == IMPORT) {
      stats += `import`()
    }
    while (in.token != EOF) {
      val mods = this.mods()
      in.token match {
        case CLASS =>
          val modClass = atPos(in.offset)(ModClass())
          in.nextToken()
          defnClass(atPos(mods.pos.start)(Mods(mods.trees :+ modClass)))
        case ENUM =>
          defnEnum(mods)
        case INTERFACE =>
          val modInterface = atPos(in.offset)(ModInterface())
          in.nextToken()
          defnClass(atPos(mods.pos.start)(Mods(mods.trees :+ modInterface)))
        case _ =>
          val errOffset = in.offset
          reportOffset(errOffset, ExpectedStartOfDefinition)
      }
    }
    stats.result
  }
}

// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse.scala

import rsc.lexis.scala._
import rsc.report._
import rsc.syntax._

trait Sources {
  self: Parser =>

  def source(): Source = {
    val start = in.offset
    atPos(start)(Source(sourceStats()))
  }

  private def sourceStats(): List[Stat] = banEscapingWildcards {
    val stats = List.newBuilder[Stat]
    while (in.token == SEMI) in.nextToken()
    val start = in.offset
    val mods = atPos(start)(Mods(Nil))
    if (in.token == PACKAGE) {
      in.nextToken()
      if (in.token == OBJECT) {
        val start = in.offset
        in.nextToken()
        stats += defnPackageObject(mods)
        if (in.token != EOF) {
          acceptStatSep()
          stats ++= packageStats()
        }
      } else {
        val id = termPath()
        newLineOptWhenFollowedBy(LBRACE)
        if (in.token == EOF) {
          stats += atPos(start)(DefnPackage(id, Nil))
        } else if (in.token == LBRACE) {
          stats += inBraces(atPos(start)(DefnPackage(id, packageStats())))
          acceptStatSepUnlessAtEnd()
          stats ++= packageStats()
        } else {
          acceptStatSep()
          stats += atPos(start)(DefnPackage(id, sourceStats()))
        }
      }
    } else {
      stats ++= packageStats()
    }
    stats.result
  }

  private def packageStats(): List[Stat] = banEscapingWildcards {
    val stats = List.newBuilder[Stat]
    while (!in.token.isStatSeqEnd) {
      if (in.token == IMPORT) {
        stats += `import`()
      } else if (in.token.isPackageDefnIntro) {
        val start = in.offset
        val mods = defnMods(modTokens.packageDefn)
        val stat = in.token match {
          case CASECLASS =>
            val modCase = atPos(in.offset)(ModCase())
            val modClass = atPos(in.offset)(ModClass())
            in.nextToken()
            defnClass(atPos(mods.pos.start)(Mods(mods.trees :+ modCase :+ modClass)))
          case CASEOBJECT =>
            val modCase = atPos(in.offset)(ModCase())
            in.nextToken()
            defnObject(atPos(mods.pos.start)(Mods(mods.trees :+ modCase)))
          case CLASS =>
            val modClass = atPos(in.offset)(ModClass())
            in.nextToken()
            defnClass(atPos(mods.pos.start)(Mods(mods.trees :+ modClass)))
          case OBJECT =>
            in.nextToken()
            defnObject(mods)
          case PACKAGE =>
            in.nextToken()
            if (in.token == OBJECT) {
              in.nextToken()
              defnPackageObject(mods)
            } else {
              val id = termPath()
              newLineOptWhenFollowedBy(LBRACE)
              inBraces(atPos(start)(DefnPackage(id, packageStats)))
            }
          case TRAIT =>
            val modTrait = atPos(in.offset)(ModTrait())
            in.nextToken()
            defnClass(atPos(mods.pos.start)(Mods(mods.trees :+ modTrait)))
          case _ =>
            val errOffset = in.offset
            reportOffset(errOffset, ExpectedStartOfDefinition)
            atPos(errOffset)(errorStat())
        }
        stats += stat
      } else if (!in.token.isStatSep) {
        if (in.token == CASE) {
          reportOffset(in.offset, IllegalStartOfDefinition)
        } else {
          reportOffset(in.offset, ExpectedClassOrObjectDefinition)
        }
      }
      acceptStatSepUnlessAtEnd()
    }
    stats.result
  }
}

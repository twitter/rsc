// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse

import rsc.lexis._
import rsc.report._
import rsc.syntax._

trait Sources {
  self: Parser =>

  def source(): Source = {
    val start = in.offset
    atPos(start)(Source(sourceStats()))
  }

  private def sourceStats(): List[Stat] = {
    val stats = List.newBuilder[Stat]
    while (in.token == SEMI) in.nextToken()
    val start = in.offset
    if (in.token == PACKAGE) {
      in.nextToken()
      if (in.token == OBJECT) {
        crash("package objects")
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

  private def packageStats(): List[Stat] = {
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
            in.nextToken()
            defnClass(start, modCase +: mods)
          case CASEOBJECT =>
            val modCase = atPos(in.offset)(ModCase())
            in.nextToken()
            defnObject(start, modCase +: mods)
          case CLASS =>
            in.nextToken()
            defnClass(start, mods)
          case OBJECT =>
            in.nextToken()
            defnObject(start, mods)
          case PACKAGE =>
            in.nextToken()
            if (in.token == OBJECT) {
              crash("package objects")
            } else {
              val id = termPath()
              newLineOptWhenFollowedBy(LBRACE)
              inBraces(atPos(start)(DefnPackage(id, packageStats)))
            }
          case TRAIT =>
            in.nextToken()
            defnTrait(start, mods)
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

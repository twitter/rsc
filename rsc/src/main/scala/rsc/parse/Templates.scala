// Copyright (c) 2017 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse

import rsc.lexis._
import rsc.report._
import rsc.syntax._

trait Templates {
  self: Parser =>

  // NOTE: Template is no longer a tree, but we do need some way
  // to avoid duplication in parser logic, so it made a reappearance.
  case class Template(inits: List[Init], stats: Option[List[Stat]])

  def defnTemplate(ctx: TemplateContext): Template = {
    if (in.token == EXTENDS) {
      in.nextToken()
      newLineOptWhenFollowedBy(LBRACE)
      if (in.token == LBRACE) {
        unsupported("early definitions")
      } else {
        val inits = templateInits()
        templateBraces(ctx, inits)
      }
    } else {
      val inits = Nil
      templateBraces(ctx, inits)
    }
  }

  def newTemplate(): Template = {
    if (in.token == LBRACE) {
      unsupported("early definitions")
    } else {
      val inits = templateInits()
      templateBraces(TermNewContext, inits)
    }
  }

  private def templateInits(): List[Init] = {
    tokenSeparated(WITH, templateInit)
  }

  private def templateInit(): Init = {
    val start = in.offset
    val tpt = annotTpt()
    val args = {
      if (in.token != LPAREN) {
        unsupported("nullary argument lists")
      }
      val result = termArgs()
      if (in.token == LPAREN) {
        unsupported("multiple argument lists")
      }
      result
    }
    atPos(start)(Init(tpt, args))
  }

  private def templateBraces(
      ctx: TemplateContext,
      inits: List[Init]): Template = {
    newLineOptWhenFollowedBy(LBRACE)
    if (in.token == LBRACE) {
      inBraces {
        val stats = List.newBuilder[Stat]
        var exitOnError = false
        while (!in.token.isStatSeqEnd && !exitOnError) {
          if (in.token == IMPORT) {
            stats += `import`()
          } else if (in.token.isTermIntro) {
            stats += term()
          } else if (in.token.isTemplateDefnIntro) {
            val start = in.offset
            val mods = defnMods(modTokens.templateDefn)
            val stat = in.token match {
              case CASECLASS =>
                if (ctx == DefnObjectContext) {
                  val modCase = atPos(in.offset)(ModCase())
                  in.nextToken()
                  defnClass(start, modCase +: mods)
                } else {
                  unsupported("inner classes")
                }
              case CASEOBJECT =>
                if (ctx == DefnObjectContext) {
                  val modCase = atPos(in.offset)(ModCase())
                  in.nextToken()
                  defnClass(start, modCase +: mods)
                } else {
                  unsupported("inner objects")
                }
              case CLASS =>
                if (ctx == DefnObjectContext) {
                  in.nextToken()
                  defnClass(start, mods)
                } else {
                  unsupported("inner classes")
                }
              case DEF =>
                in.nextToken()
                defnDef(start, mods)
              case OBJECT =>
                if (ctx == DefnObjectContext) {
                  in.nextToken()
                  defnObject(start, mods)
                } else {
                  unsupported("inner objects")
                }
              case TRAIT =>
                if (ctx == DefnObjectContext) {
                  in.nextToken()
                  defnTrait(start, mods)
                } else {
                  unsupported("inner traits")
                }
              case TYPE =>
                in.nextToken()
                defnType(start, mods)
              case VAL =>
                val modVal = atPos(in.offset)(ModVal())
                in.nextToken()
                defnField(start, mods :+ modVal)
              case VAR =>
                val modVar = atPos(in.offset)(ModVar())
                in.nextToken()
                defnField(start, mods :+ modVar)
              case _ =>
                val errOffset = in.offset
                reportOffset(errOffset, ExpectedStartOfDefinition)
                atPos(errOffset)(errorStat())
            }
            stats += stat
          } else if (!in.token.isStatSep) {
            exitOnError = in.token.mustStartStat
            reportOffset(in.offset, IllegalStartOfDefinition)
          }
          acceptStatSepUnlessAtEnd()
        }
        Template(inits, Some(stats.result))
      }
    } else {
      val stats = None
      Template(inits, stats)
    }
  }
}

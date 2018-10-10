// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse.scala

import rsc.input._
import rsc.lexis.scala._
import rsc.report._
import rsc.syntax._
import rsc.util._

trait Templates {
  self: Parser =>

  // NOTE: Template is no longer a tree, but we do need some way
  // to avoid duplication in parser logic, so it made a reappearance.
  case class Template(
      earlies: List[Stat],
      inits: List[Init],
      self: Option[Self],
      stats: Option[List[Stat]])

  def defnTemplate(): Template = {
    if (in.token == EXTENDS) {
      in.nextToken()
      newLineOptWhenFollowedBy(LBRACE)
      if (in.token == LBRACE) {
        val template = templateBraces(Nil, Nil)
        if (in.token == WITH) {
          in.nextToken()
          template.self.foreach(self => reportPos(self.pos, IllegalSelf))
          val earlies = template.stats.getOrElse(Nil)
          val inits = templateInits()
          templateBraces(earlies, inits)
        } else {
          template
        }
      } else {
        val earlies = Nil
        val inits = templateInits()
        templateBraces(earlies, inits)
      }
    } else {
      val earlies = Nil
      val inits = Nil
      templateBraces(earlies, inits)
    }
  }

  def newTemplate(): Template = {
    newLineOptWhenFollowedBy(LBRACE)
    if (in.token == LBRACE) {
      val earlyTemplate = templateBraces(Nil, Nil)
      if (in.token == WITH) {
        in.nextToken()
        val earlies = earlyTemplate.stats.getOrElse(Nil)
        val inits = templateInits()
        templateBraces(earlies, inits)
      } else {
        earlyTemplate
      }
    } else {
      val inits = templateInits()
      newLineOptWhenFollowedBy(LBRACE)
      if (in.token == LBRACE) {
        val earlies = Nil
        templateBraces(earlies, inits)
      } else {
        val earlies = Nil
        Template(earlies, inits, None, None)
      }
    }
  }

  private def templateInits(): List[Init] = {
    withSeparated(templateInit)
  }

  private def templateInit(): Init = {
    val initstart = in.offset
    val tpt = annotTpt()
    val idstart = in.offset
    val argss = termArgss()
    val init = atPos(initstart)(Init(tpt, argss))
    init.id.pos = Position(input, idstart, idstart)
    init
  }

  private def templateBraces(earlies: List[Stat], inits: List[Init]): Template = {
    banEscapingWildcards {
      newLineOptWhenFollowedBy(LBRACE)
      if (in.token == LBRACE) {
        inBraces {
          var self: Option[Self] = None
          val stats = List.newBuilder[Stat]
          if (in.token.isTermIntro) {
            val snapshot = in.snapshot()
            val firstStat = term1(InTemplate)
            if (in.token == ARROW) {
              in.restore(snapshot)
              val start = in.offset
              val id = {
                if (in.token == ID) {
                  termId()
                } else if (in.token == THIS || in.token == USCORE) {
                  in.nextToken()
                  atPos(start)(anonId())
                } else {
                  val errOffset = in.offset
                  reportOffset(errOffset, IllegalSelf)
                  in.nextToken()
                  atPos(errOffset)(TermId(gensym.error()))
                }
              }
              val tpt = {
                if (in.token == COLON) {
                  in.nextToken()
                  Some(infixTpt())
                } else {
                  None
                }
              }
              accept(ARROW)
              self = Some(atPos(start)(Self(id, tpt)))
            } else {
              stats += firstStat
              acceptStatSepUnlessAtEnd()
            }
          }
          var exitOnError = false
          while (!in.token.isStatSeqEnd && !exitOnError) {
            if (in.token == IMPORT) {
              stats += `import`()
            } else if (in.token.isTermIntro) {
              stats += term1(InTemplate)
            } else if (in.token.isTemplateDefnIntro) {
              val start = in.offset
              val mods = defnMods(modTokens.templateDefn)
              val stat = in.token match {
                case CASECLASS =>
                  val modCase = atPos(in.offset)(ModCase())
                  val modClass = atPos(in.offset)(ModClass())
                  in.nextToken()
                  defnClass(mods :+ modCase :+ modClass)
                case CASEOBJECT =>
                  val modCase = atPos(in.offset)(ModCase())
                  in.nextToken()
                  defnObject(mods :+ modCase)
                case CLASS =>
                  val modClass = atPos(in.offset)(ModClass())
                  in.nextToken()
                  defnClass(mods :+ modClass)
                case DEF =>
                  in.nextToken()
                  defnDef(mods)
                case OBJECT =>
                  in.nextToken()
                  defnObject(mods)
                case TRAIT =>
                  val modTrait = atPos(in.offset)(ModTrait())
                  in.nextToken()
                  defnClass(mods :+ modTrait)
                case TYPE =>
                  in.nextToken()
                  defnType(mods)
                case VAL =>
                  val modVal = atPos(in.offset)(ModVal())
                  in.nextToken()
                  defnVal(mods :+ modVal)
                case VAR =>
                  val modVar = atPos(in.offset)(ModVar())
                  in.nextToken()
                  defnVar(mods :+ modVar)
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
          Template(earlies, inits, self, Some(stats.result))
        }
      } else {
        val self = None
        val stats = None
        Template(earlies, inits, self, stats)
      }
    }
  }
}

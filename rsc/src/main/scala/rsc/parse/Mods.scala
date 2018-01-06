// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse

import scala.collection.immutable.BitSet
import rsc.lexis._
import rsc.report._
import rsc.syntax._

trait Mods {
  self: Parser =>

  def defnMods(modTokens: BitSet): List[Mod] = {
    val annots = this.annots(skipNewLines = true)
    val flags = this.defnFlags(modTokens)
    annots ++ flags
  }

  def primaryCtorMods(): List[Mod] = {
    val annots = this.annots(skipNewLines = false)
    val flags = this.primaryCtorFlags()
    annots ++ flags
  }

  def termParamMods(ctx: ParamContext): List[Mod] = {
    val annots = this.annots(skipNewLines = false)
    val flags = this.termParamFlags(ctx)
    annots ++ flags
  }

  def typeParamMods(ctx: ParamContext): List[Mod] = {
    val annots = this.annots(skipNewLines = false)
    val flags = this.typeParamFlags(ctx)
    annots ++ flags
  }

  def termAnnotateMods(): List[Mod] = {
    annots(skipNewLines = false)
  }

  def typeAnnotateMods(): List[Mod] = {
    annots(skipNewLines = false)
  }

  private def annots(skipNewLines: Boolean): List[Mod] = {
    if (skipNewLines) {
      newLineOptWhenFollowedBy(AT)
    }
    if (in.token == AT) {
      unsupported("annotations")
    } else {
      Nil
    }
  }

  private def annotInit(): Init = {
    val start = in.offset
    val tpts = simpleTpt()
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
    atPos(start)(Init(tpts, args))
  }

  private def defnFlags(modTokens: BitSet): List[Mod] = {
    def addFlag(flags: List[Mod], flag: Mod): List[Mod] = {
      val isRepeated = flags.exists(_.productPrefix == flag.productPrefix)
      if (isRepeated) reportOffset(in.offset, RepeatedModifier)
      flags :+ flag
    }
    def loop(flags: List[Mod]): List[Mod] = {
      if (modTokens.contains(in.token)) {
        def within(): Option[Path] = {
          if (in.token == LBRACKET) {
            inBrackets {
              if (in.token == THIS) {
                val id = anonId()
                val start = in.offset
                in.nextToken()
                Some(atPos(start)(TermThis(id)))
              } else {
                Some(someId())
              }
            }
          } else {
            None
          }
        }
        val start = in.offset
        val flag = {
          in.token match {
            case ABSTRACT =>
              in.nextToken()
              atPos(start)(ModAbstract())
            case FINAL =>
              in.nextToken()
              atPos(start)(ModFinal())
            case LAZY =>
              in.nextToken()
              atPos(start)(ModLazy())
            case IMPLICIT =>
              unsupported("implicit parameters")
            case OVERRIDE =>
              in.nextToken()
              atPos(start)(ModOverride())
            case PRIVATE =>
              in.nextToken()
              atPos(start)(ModPrivate(within()))
            case PROTECTED =>
              in.nextToken()
              atPos(start)(ModProtected(within()))
            case SEALED =>
              in.nextToken()
              atPos(start)(ModSealed())
            case _ =>
              unreachable(tokenRepl(in.token))
          }
        }
        loop(addFlag(flags, flag))
      } else {
        flags
      }
    }
    loop(Nil)
  }

  private def primaryCtorFlags(): List[Mod] = {
    defnFlags(modTokens.primaryCtor)
  }

  private def termParamFlags(ownedBy: ParamContext): List[Mod] = {
    val flags = defnFlags(modTokens.termParam)
    val extraFlags = {
      if (in.token == VAL) {
        val start = in.offset
        in.nextToken()
        List(atPos(start)(ModVal()))
      } else if (in.token == VAR) {
        val start = in.offset
        in.nextToken()
        List(atPos(start)(ModVar()))
      } else {
        Nil
      }
    }
    if (flags.nonEmpty && extraFlags.isEmpty) {
      reportOffset(in.offset, IllegalModifier)
    }
    flags ++ extraFlags
  }

  private def typeParamFlags(ownedBy: ParamContext): List[Mod] = {
    if (ownedBy.allowsVariance) {
      if (in.token == ID && in.idValue == "+") {
        val start = in.offset
        in.nextToken()
        List(atPos(start)(ModCovariant()))
      } else if (in.token == ID && in.idValue == "-") {
        val start = in.offset
        in.nextToken()
        List(atPos(start)(ModContravariant()))
      } else Nil
    } else {
      Nil
    }
  }
}

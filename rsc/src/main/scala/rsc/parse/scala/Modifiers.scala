// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse.scala

import rsc.inputs._
import rsc.lexis.scala._
import rsc.report._
import rsc.syntax._
import scala.collection.immutable.BitSet

trait Modifiers {
  self: Parser =>

  def defnMods(modTokens: BitSet): Mods = {
    val start = in.offset
    val annots = this.annots(skipNewLines = true, exactlyOneArglist = false)
    val flags = this.defnFlags(modTokens)
    atPos(start)(Mods(annots ++ flags))
  }

  def primaryCtorMods(): Mods = {
    val start = in.offset
    val annots = this.annots(skipNewLines = false, exactlyOneArglist = true)
    val flags = this.primaryCtorFlags()
    atPos(start)(Mods(annots ++ flags))
  }

  def paramMods(ctx: ParamContext): Mods = {
    val start = in.offset
    val annots = this.annots(skipNewLines = false, exactlyOneArglist = false)
    val flags = this.paramFlags(ctx)
    atPos(start)(Mods(annots ++ flags))
  }

  def typeParamMods(ctx: ParamContext): Mods = {
    val start = in.offset
    val annots = this.annots(skipNewLines = true, exactlyOneArglist = false)
    val flags = this.typeParamFlags(ctx)
    atPos(start)(Mods(annots ++ flags))
  }

  def termAnnotateMods(): Mods = {
    val start = in.offset
    atPos(start)(Mods(annots(skipNewLines = false, exactlyOneArglist = false)))
  }

  def typeAnnotateMods(): Mods = {
    val start = in.offset
    atPos(start)(Mods(annots(skipNewLines = false, exactlyOneArglist = false)))
  }

  private def annots(skipNewLines: Boolean, exactlyOneArglist: Boolean): List[Mod] = {
    if (skipNewLines) {
      newLineOpt()
    }
    if (in.token == AT) {
      val start = in.offset
      in.nextToken()
      val init = annotInit(exactlyOneArglist)
      val annot = atPos(start)(ModAnnotation(init))
      annot :: annots(skipNewLines, exactlyOneArglist)
    } else {
      Nil
    }
  }

  private def annotInit(exactlyOneArglist: Boolean): Init = {
    val initstart = in.offset
    val tpt = simpleTpt()
    val idstart = in.offset
    val argss = {
      if (exactlyOneArglist) List(termArgs())
      else termArgss()
    }
    val init = atPos(initstart)(Init(tpt, argss))
    init.id.pos = Position(input, idstart, idstart)
    init
  }

  private def defnFlags(modTokens: BitSet): List[Mod] = {
    def addFlag(flags: List[Mod], flag: Mod): List[Mod] = {
      val isRepeated = flags.exists(_.productPrefix == flag.productPrefix)
      if (isRepeated) reportOffset(in.offset, RepeatedModifier)
      flags :+ flag
    }
    def loop(flags: List[Mod]): List[Mod] = {
      if (modTokens.contains(NEWLINE)) {
        newLineOpt()
      }
      if (modTokens.contains(in.token)) {
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
              in.nextToken()
              atPos(start)(ModImplicit())
            case OVERRIDE =>
              in.nextToken()
              atPos(start)(ModOverride())
            case PRIVATE =>
              in.nextToken()
              if (in.token == LBRACKET) {
                inBrackets {
                  if (in.token == THIS) {
                    in.nextToken()
                    atPos(start)(ModPrivateThis())
                  } else {
                    atPos(start)(ModPrivateWithin(someId()))
                  }
                }
              } else {
                atPos(start)(ModPrivate())
              }
            case PROTECTED =>
              in.nextToken()
              if (in.token == LBRACKET) {
                inBrackets {
                  if (in.token == THIS) {
                    in.nextToken()
                    atPos(start)(ModProtectedThis())
                  } else {
                    atPos(start)(ModProtectedWithin(someId()))
                  }
                }
              } else {
                atPos(start)(ModProtected())
              }
            case SEALED =>
              in.nextToken()
              atPos(start)(ModSealed())
            case _ =>
              crash(tokenRepl(in.token))
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

  private def paramFlags(ownedBy: ParamContext): List[Mod] = {
    val flags = defnFlags(modTokens.param)
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

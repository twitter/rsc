// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse.java

import rsc.lexis.java._
import rsc.syntax._
import rsc.util._

trait Modifiers {
  self: Parser =>

  def mods(): Mods = {
    def loop(): List[Mod] = {
      val start = in.offset
      val mod = in.token match {
        case ABSTRACT =>
          in.nextToken()
          atPos(start)(ModAbstract())
        case AT =>
          in.nextToken()
          if (in.token == INTERFACE) {
            atPos(start)(ModAnnotationInterface())
          } else {
            val initStart = in.offset
            val tpt = annotationTpt()
            val argss = {
              if (in.token == LPAREN) skipParens()
              else Nil
            }
            val init = atPos(initStart)(Init(tpt, Nil))
            atPos(start)(ModAnnotation(init))
          }
        case DEFAULT =>
          in.nextToken()
          atPos(start)(ModDefault())
        case FINAL =>
          in.nextToken()
          atPos(start)(ModFinal())
        case NATIVE =>
          in.nextToken()
          atPos(start)(ModNative())
        case PRIVATE =>
          in.nextToken()
          atPos(start)(ModPrivate())
        case PROTECTED =>
          in.nextToken()
          atPos(start)(ModProtected())
        case PUBLIC =>
          in.nextToken()
          atPos(start)(ModPublic())
        case STATIC =>
          in.nextToken()
          atPos(start)(ModStatic())
        case STRICTFP =>
          in.nextToken()
          atPos(start)(ModStrictfp())
        case SYNCHRONIZED =>
          in.nextToken()
          atPos(start)(ModSynchronized())
        case TRANSIENT =>
          in.nextToken()
          atPos(start)(ModTransient())
        case VOLATILE =>
          in.nextToken()
          atPos(start)(ModVolatile())
        case _ =>
          return Nil
      }
      mod +: loop()
    }
    val start = in.offset
    atPos(start)(Mods(loop()))
  }

  def modDims(mods: Mods): Mods = {
    if (in.token == LBRACKET) {
      val start = in.offset
      accept(LBRACKET)
      accept(RBRACKET)
      val nestedDims = modDims(atPos(in.offset)(Mods(Nil)))
      mods :+ atPos(start)(ModDims(nestedDims))
    } else {
      mods
    }
  }

  def modThrows(mods: Mods): Mods = {
    if (in.token == THROWS) {
      val start = in.offset
      in.nextToken()
      val tpts = commaSeparated(this.tpt())
      val throws = atPos(start)(ModThrows(tpts))
      mods :+ throws
    } else {
      mods
    }
  }

  private def annotationTpt(): TptPath = {
    val start = in.offset
    def loop(path: TptPath): TptPath = {
      if (in.token == DOT) {
        accept(DOT)
        val qual = path match {
          case TptId(value) =>
            atPos(path.pos)(AmbigId(value))
          case TptSelect(qual, tptId @ TptId(value)) =>
            val ambigId = atPos(tptId.pos)(AmbigId(value))
            atPos(path.pos)(AmbigSelect(qual, ambigId))
          case other =>
            crash(other)
        }
        val id = tptId()
        loop(atPos(start)(TptSelect(qual, id)))
      } else {
        path
      }
    }
    loop(tptId())
  }
}

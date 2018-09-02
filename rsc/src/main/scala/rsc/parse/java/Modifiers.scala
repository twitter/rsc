// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse.java

import rsc.lexis.java._
import rsc.syntax._

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
            skipParens()
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

  private def annotationTpt(): Tpt = {
    ???
  }
}

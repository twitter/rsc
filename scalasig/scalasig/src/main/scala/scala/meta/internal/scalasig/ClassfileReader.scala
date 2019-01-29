// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
// NOTE: This file has been partially copy/pasted from scala/scala.
package scala.meta.internal.scalasig

import java.nio.charset.StandardCharsets.UTF_8

// NOTE: Surprisingly, there is specification for this aspect of ScalaSignatures.
// https://docs.scala-lang.org/sips/picked-signatures.html.

object ClassfileReader {
  def unpackScalasig(packedScalasig: Array[String]): Array[Byte] = {
    val in = packedScalasig.flatMap(_.getBytes(UTF_8))
    val scratchpad = new Array[Byte](in.length)
    System.arraycopy(in, 0, scratchpad, 0, in.length)
    val srclen = regenerateZero(scratchpad)
    var dstlen = decode7to8(scratchpad, srclen)
    val out = new Array[Byte](dstlen)
    System.arraycopy(scratchpad, 0, out, 0, out.length)
    out
  }

  private def regenerateZero(src: Array[Byte]): Int = {
    var i = 0
    val srclen = src.length
    var j = 0
    while (i < srclen) {
      val in: Int = src(i) & 0xff
      if (in == 0xc0 && (src(i + 1) & 0xff) == 0x80) {
        src(j) = 0x7f
        i += 2
      } else if (in == 0) {
        src(j) = 0x7f
        i += 1
      } else {
        src(j) = (in - 1).toByte
        i += 1
      }
      j += 1
    }
    j
  }

  private def decode7to8(src: Array[Byte], srclen: Int): Int = {
    var i = 0
    var j = 0
    val dstlen = (srclen * 7 + 7) / 8
    while (i + 7 < srclen) {
      var out: Int = src(i).toInt
      var in: Byte = src(i + 1)
      src(j) = (out | (in & 0x01) << 7).toByte
      out = in >>> 1
      in = src(i + 2)
      src(j + 1) = (out | (in & 0x03) << 6).toByte
      out = in >>> 2
      in = src(i + 3)
      src(j + 2) = (out | (in & 0x07) << 5).toByte
      out = in >>> 3
      in = src(i + 4)
      src(j + 3) = (out | (in & 0x0f) << 4).toByte
      out = in >>> 4
      in = src(i + 5)
      src(j + 4) = (out | (in & 0x1f) << 3).toByte
      out = in >>> 5
      in = src(i + 6)
      src(j + 5) = (out | (in & 0x3f) << 2).toByte
      out = in >>> 6
      in = src(i + 7)
      src(j + 6) = (out | in << 1).toByte
      i += 8
      j += 7
    }
    if (i < srclen) {
      var out: Int = src(i).toInt
      if (i + 1 < srclen) {
        var in: Byte = src(i + 1)
        src(j) = (out | (in & 0x01) << 7).toByte; j += 1
        out = in >>> 1
        if (i + 2 < srclen) {
          in = src(i + 2)
          src(j) = (out | (in & 0x03) << 6).toByte; j += 1
          out = in >>> 2
          if (i + 3 < srclen) {
            in = src(i + 3)
            src(j) = (out | (in & 0x07) << 5).toByte; j += 1
            out = in >>> 3
            if (i + 4 < srclen) {
              in = src(i + 4)
              src(j) = (out | (in & 0x0f) << 4).toByte; j += 1
              out = in >>> 4
              if (i + 5 < srclen) {
                in = src(i + 5)
                src(j) = (out | (in & 0x1f) << 3).toByte; j += 1
                out = in >>> 5
                if (i + 6 < srclen) {
                  in = src(i + 6)
                  src(j) = (out | (in & 0x3f) << 2).toByte; j += 1
                  out = in >>> 6
                }
              }
            }
          }
        }
      }
      if (j < dstlen) src(j) = out.toByte
    }
    dstlen
  }
}

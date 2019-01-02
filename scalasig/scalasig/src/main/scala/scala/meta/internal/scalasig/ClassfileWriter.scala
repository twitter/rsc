// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
// NOTE: This file has been partially copy/pasted from scala/scala.
package scala.meta.internal.scalasig

// NOTE: While ClassfileReader is documented, ClassfileWriter is not.
// The implementation in Scalac seems to be inconsistent with the official
// documentation at: https://docs.scala-lang.org/sips/picked-signatures.html.
// * https://github.com/scala/scala/blob/v2.12.6/src/compiler/scala/tools/nsc/backend/jvm/BCodeHelpers.scala
// * https://github.com/scala/scala/blob/v2.12.6/src/reflect/scala/reflect/internal/AnnotationInfos.scala
// * https://github.com/scala/scala/blob/v2.12.6/src/reflect/scala/reflect/internal/pickling/ByteCodecs.scala

object ClassfileWriter {
  def packScalasig(unpackedScalasig: Array[Byte]): Array[String] = {
    val ubytes = mapToNextModSevenBits(encode8to7(unpackedScalasig))
    if (needsScalaLongSignature(ubytes)) ubytesToArray(ubytes)
    else Array(ubytesToString(ubytes))
  }

  private def encode8to7(src: Array[Byte]): Array[Byte] = {
    val srclen = src.length
    val dstlen = (srclen * 8 + 6) / 7
    val dst = new Array[Byte](dstlen)
    var i = 0
    var j = 0
    while (i + 6 < srclen) {
      var in: Int = src(i) & 0xff
      dst(j) = (in & 0x7f).toByte
      var out: Int = in >>> 7
      in = src(i + 1) & 0xff
      dst(j + 1) = (out | (in << 1) & 0x7f).toByte
      out = in >>> 6
      in = src(i + 2) & 0xff
      dst(j + 2) = (out | (in << 2) & 0x7f).toByte
      out = in >>> 5
      in = src(i + 3) & 0xff
      dst(j + 3) = (out | (in << 3) & 0x7f).toByte
      out = in >>> 4
      in = src(i + 4) & 0xff
      dst(j + 4) = (out | (in << 4) & 0x7f).toByte
      out = in >>> 3
      in = src(i + 5) & 0xff
      dst(j + 5) = (out | (in << 5) & 0x7f).toByte
      out = in >>> 2
      in = src(i + 6) & 0xff
      dst(j + 6) = (out | (in << 6) & 0x7f).toByte
      out = in >>> 1
      dst(j + 7) = out.toByte
      i += 7
      j += 8
    }
    if (i < srclen) {
      var in: Int = src(i) & 0xff
      dst(j) = (in & 0x7f).toByte; j += 1
      var out: Int = in >>> 7
      if (i + 1 < srclen) {
        in = src(i + 1) & 0xff
        dst(j) = (out | (in << 1) & 0x7f).toByte; j += 1
        out = in >>> 6
        if (i + 2 < srclen) {
          in = src(i + 2) & 0xff
          dst(j) = (out | (in << 2) & 0x7f).toByte; j += 1
          out = in >>> 5
          if (i + 3 < srclen) {
            in = src(i + 3) & 0xff
            dst(j) = (out | (in << 3) & 0x7f).toByte; j += 1
            out = in >>> 4
            if (i + 4 < srclen) {
              in = src(i + 4) & 0xff
              dst(j) = (out | (in << 4) & 0x7f).toByte; j += 1
              out = in >>> 3
              if (i + 5 < srclen) {
                in = src(i + 5) & 0xff
                dst(j) = (out | (in << 5) & 0x7f).toByte; j += 1
                out = in >>> 2
              }
            }
          }
        }
      }
      if (j < dstlen) dst(j) = out.toByte
    }
    dst
  }

  private def mapToNextModSevenBits(src: Array[Byte]): Array[Byte] = {
    var i = 0
    val srclen = src.length
    while (i < srclen) {
      val in = src(i)
      src(i) = (if (in == 0x7f) 0.toByte else (in + 1).toByte)
      i += 1
    }
    src
  }

  private def needsScalaLongSignature(src: Array[Byte]): Boolean = {
    var i = 0
    var numZeros = 0
    while (i < src.length) {
      if (src(i) == 0) numZeros += 1
      i += 1
    }
    (src.length + numZeros) >= 65536
  }

  private def ubytesToString(ubytes: Array[Byte]): String = {
    val chars = new Array[Char](ubytes.length)
    var i = 0
    while (i < ubytes.length) {
      val b: Byte = ubytes(i)
      assert((b & ~0x7f) == 0)
      chars(i) = b.asInstanceOf[Char]
      i += 1
    }
    new String(chars)
  }

  private def ubytesToArray(ubytes: Array[Byte]): Array[String] = {
    var strs: List[String] = Nil
    var prevOffset = 0
    var offset = 0
    var encLength = 0
    while (offset < ubytes.length) {
      val deltaEncLength = if (ubytes(offset) == 0) 2 else 1
      val newEncLength = encLength.toLong + deltaEncLength
      if (newEncLength >= 65535) {
        val ba = ubytes.slice(prevOffset, offset)
        strs ::= ubytesToString(ba)
        encLength = 0
        prevOffset = offset
      } else {
        encLength += deltaEncLength
        offset += 1
      }
    }
    if (prevOffset < offset) {
      assert(offset == ubytes.length)
      val ba = ubytes.slice(prevOffset, offset)
      strs ::= ubytesToString(ba)
    }
    strs.reverse.toArray
  }
}

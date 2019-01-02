// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
// NOTE: This file has been partially copy/pasted from scala/scala.
package scala.meta.internal.scalasig

import java.nio.charset.StandardCharsets.UTF_8
import java.util.Arrays

// NOTE: There is no specification for this aspect of ScalaSignatures.
// The best that we have is the Scala compiler source code:
// https://github.com/scala/scala/blob/v2.12.6/src/reflect/scala/reflect/internal/pickling/PickleBuffer.scala

class ScalasigWriter {
  private var bytes = new Array[Byte](1024)
  var offset = 0

  def writeByte(x: Int): Unit = {
    val requestedLen = offset + 1
    if (requestedLen > bytes.length) {
      val bytes1 = new Array[Byte](requestedLen * 2)
      Array.copy(bytes, 0, bytes1, 0, offset)
      bytes = bytes1
    }
    bytes(offset) = x.toByte
    offset += 1
  }

  def writeString(x: String): Unit = {
    val utfBytes = x.getBytes(UTF_8)
    val requestedLen = offset + utfBytes.length
    if (requestedLen > bytes.length) {
      val bytes1 = new Array[Byte](requestedLen * 2)
      Array.copy(bytes, 0, bytes1, 0, offset)
      bytes = bytes1
    }
    System.arraycopy(utfBytes, 0, bytes, offset, utfBytes.length)
    offset += utfBytes.length
  }

  // NOTE: Write a 32-bit number as a base-128 varint.
  // To learn more what a varint means, check out:
  // https://developers.google.com/protocol-buffers/docs/encoding#varints
  def writeVarint(x: Int): Unit = {
    writeVarlong(x.toLong & 0x00000000FFFFFFFFL)
  }

  // NOTE: Write a 32-bit number as a base-128 varint at offset.
  // If number takes more than one byte, shift rest of array to make space.
  // To learn more what a varint means, check out:
  // https://developers.google.com/protocol-buffers/docs/encoding#varints
  def patchVarint(pos: Int, x: Int) {
    def patchPrefix(x: Int) {
      writeByte(0)
      Array.copy(bytes, pos, bytes, pos + 1, offset - (pos + 1))
      bytes(pos) = ((x & 0x7f) | 0x80).toByte
      val y = x >>> 7
      if (y != 0) patchPrefix(y)
    }
    bytes(pos) = (x & 0x7f).toByte
    val y = x >>> 7
    if (y != 0) patchPrefix(y)
  }

  // NOTE: Write a 64-bit number as a base-128 varint.
  // To learn more what a varint means, check out:
  // https://developers.google.com/protocol-buffers/docs/encoding#varints
  def writeVarlong(x: Long): Unit = {
    def writePrefix(x: Long) {
      val y = x >>> 7
      if (y != 0L) writePrefix(y)
      writeByte(((x & 0x7f) | 0x80).toInt)
    }
    val y = x >>> 7
    if (y != 0L) writePrefix(y)
    writeByte((x & 0x7f).toInt)
  }

  // NOTE: Write a 64-bit number as a base-256 number.
  def writeNumber(x: Long): Unit = {
    val y = x >> 8
    val z = x & 0xff
    if (-y != (z >> 7)) writeNumber(y)
    writeByte(z.toInt)
  }

  def toByteArray: Array[Byte] = {
    Arrays.copyOfRange(bytes, 0, offset)
  }
}

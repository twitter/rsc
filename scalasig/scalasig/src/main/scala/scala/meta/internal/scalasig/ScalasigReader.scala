// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
// NOTE: This file has been partially copy/pasted from scala/scala.
package scala.meta.internal.scalasig

import java.nio.charset.StandardCharsets.UTF_8

// NOTE: There is no specification for this aspect of ScalaSignatures.
// The best that we have is the Scala compiler source code:
// https://github.com/scala/scala/blob/v2.12.6/src/reflect/scala/reflect/internal/pickling/PickleBuffer.scala

class ScalasigReader(bytes: Array[Byte]) {
  var offset = 0

  def atOffset[T](offset: Int)(fn: => T): T = {
    val savedOffset = this.offset
    this.offset = offset
    val result = fn
    this.offset = savedOffset
    result
  }

  def readByte(): Int = {
    val result = bytes(offset)
    offset += 1
    result.toInt
  }

  def readString(len: Int): String = {
    val result = new String(bytes, offset, len, UTF_8)
    offset += len
    result
  }

  // NOTE: Read a base-128 varint, coerce it into 32 bits.
  // To learn more what a varint means, check out:
  // https://developers.google.com/protocol-buffers/docs/encoding#varints
  def readVarint(): Int = {
    readVarlong().toInt
  }

  // NOTE: Read a base-128 varint, coerce it into 64 bits.
  // To learn more what a varint means, check out:
  // https://developers.google.com/protocol-buffers/docs/encoding#varints
  def readVarlong(): Long = {
    var b = 0L
    var x = 0L
    do {
      b = readByte().toLong
      x = (x << 7) + (b & 0x7f)
    } while ((b & 0x80) != 0L)
    x
  }

  // NOTE: Read a base-256 number, coerce it into 64 bits.
  // The number of bytes to read is encoded in a base-128 varint.
  def readNumber(): Long = {
    val len = readVarint()
    var x = 0L
    var i = 0
    while (i < len) {
      x = (x << 8) + (readByte() & 0xff)
      i += 1
    }
    val leading = 64 - (len << 3)
    x << leading >> leading
  }
}

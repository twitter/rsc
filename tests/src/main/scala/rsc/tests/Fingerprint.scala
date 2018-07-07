// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.tests

import java.nio._
import java.nio.file._
import java.security.MessageDigest
import javax.xml.bind.DatatypeConverter
import scala.collection.JavaConverters._

// NOTE: This implementation is mostly sound, but allows for some rarely occuring:
//   * False positives (e.g. when someone tampers with timestamps)
//   * False negatives (e.g. when some files have changed, but it doesn't matter)
//
// However, for our use cases - speeding up repeated test invocations on
// developer laptops - I think it's not a problem.
//
// False positives may lead to incorrect test results, but:
// 1) test runs on CI are guaranteed to be correct,
// 2) prerequisites for false positives are so exotic that we may disregard
// them as an acceptable trade-off for speeding up test runs.
//
// False negatives are annoying, because they flush caches, but they are also
// quite rare for our test cases and, best of all, they are completely harmless.

class Fingerprint private (val value: String) {
  override def equals(that: Any): Boolean = that match {
    case that: Fingerprint => this.value == that.value
    case _ => false
  }
  override def hashCode: Int = value.hashCode
  override def toString: String = value.toString
}

object Fingerprint {
  def apply(paths: Path*): Fingerprint = {
    apply(paths.toList)
  }

  def apply(paths: List[Path]): Fingerprint = {
    val digest = MessageDigest.getInstance("md5")
    paths.map { path =>
      val children = Files.walk(path).iterator.asScala.toList
      children.map { child =>
        digest.update(child.toString.getBytes())
        digest.update(Files.getLastModifiedTime(child).toMillis.toBytes)
        if (!Files.isDirectory(child)) digest.update(Files.readAllBytes(child))
      }
    }
    val hash = DatatypeConverter.printHexBinary(digest.digest())
    new Fingerprint(hash)
  }

  private implicit class LongOps(value: Long) {
    def toBytes: Array[Byte] = {
      val buf = ByteBuffer.allocate(java.lang.Long.BYTES)
      buf.putLong(value)
      buf.array()
    }
  }
}

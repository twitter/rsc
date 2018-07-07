// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scala.meta.scalasig

import java.io._
import java.net._
import java.nio.file._
import scala.language.implicitConversions

sealed trait Binary {
  def openStream(): InputStream
}

object Binary {
  implicit def uriBinary(uri: URI): Binary = UriBinary(uri)
  implicit def pathBinary(path: Path): Binary = PathBinary(path)
  implicit def bytesBinary(bytes: Array[Byte]): Binary = BytesBinary(bytes)
}

case object NoBinary extends Binary {
  def openStream(): InputStream = throw new UnsupportedOperationException()
  override def toString: String = "<none>"
}

case class UriBinary(uri: URI) extends Binary {
  def openStream(): InputStream = uri.toURL.openStream()
  override def toString: String = uri.toURL.toString
}

case class PathBinary(path: Path) extends Binary {
  def openStream(): InputStream = Files.newInputStream(path)
  override def toString: String = path.toString
}

case class BytesBinary(bytes: Array[Byte]) extends Binary {
  def openStream(): InputStream = new ByteArrayInputStream(bytes)
  override def toString: String = "<bytes>"
}

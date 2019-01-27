// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scala.meta.scalasig

import org.objectweb.asm.tree._

sealed trait Payload
case object NoPayload extends Payload
case class JavaPayload(node: ClassNode) extends Payload
case class ScalaPayload(scalasigBytes: Array[Byte]) extends Payload

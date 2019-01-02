// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scala.meta.internal.scalasig

import org.objectweb.asm._

final class PickleMarker extends CustomAttribute("ScalaSig", PickleMarker.bytes)

object PickleMarker {
  val bytes: Array[Byte] = {
    val writer = new ScalasigWriter
    writer.writeVarint(MajorVersion)
    writer.writeVarint(MinorVersion)
    writer.writeVarint(0)
    writer.toByteArray
  }
}

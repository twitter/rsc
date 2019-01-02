// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.scalasig

import scala.meta.internal.{semanticdb => s}

sealed trait Pre
case object NoPre extends Pre
case class SomePre(stpe: s.Type) extends Pre

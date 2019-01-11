// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.semantics

import rsc.syntax._
import scala.meta.internal.{semanticdb => s}

sealed trait Metadata
case class OutlineMetadata(outline: Outline) extends Metadata
case class ClasspathMetadata(info: s.SymbolInformation) extends Metadata
case object NoMetadata extends Metadata

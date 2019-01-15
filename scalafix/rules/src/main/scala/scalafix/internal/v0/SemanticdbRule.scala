// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scalafix.internal.v0

import scalafix.v0._

abstract class SemanticdbRule(legacyIndex: SemanticdbIndex, name: String)
    extends SemanticRule(legacyIndex, name) {

  lazy val index: DocumentIndex = {
    new DocumentIndex(legacyIndex.asInstanceOf[LegacySemanticdbIndex])
  }
}

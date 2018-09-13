// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scalafix.internal.v0

import scalafix.v0._

abstract class SemanticdbRule(legacyIndex: SemanticdbIndex, name: String)
    extends SemanticRule(legacyIndex, name) {

  lazy val index: DocumentIndex = {
    val scalafixDoc = legacyIndex.asInstanceOf[LegacySemanticdbIndex].doc
    val semanticdbDoc = scalafixDoc.internal.textDocument
    new DocumentIndex(semanticdbDoc)
  }
}

// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scalafix.internal.v0

import scalafix.v0._

abstract class SemanticdbRule(legacyIndex: SemanticdbIndex, name: String, better: Boolean)
    extends SemanticRule(legacyIndex, name) {

  lazy val symbols: DocumentSymbols = {
    val scalafixIndex = legacyIndex.asInstanceOf[LegacySemanticdbIndex]
    if (better) {
      BetterDocumentSymbols(scalafixIndex)
    } else {
      val scalafixDoc = scalafixIndex.doc
      val semanticdbDoc = scalafixDoc.internal.textDocument
      RegularDocumentSymbols(semanticdbDoc)
    }
  }
}

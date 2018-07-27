// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scalafix.v0

import scalafix.internal.patch._

abstract class SemanticdbRule(legacyIndex: SemanticdbIndex, name: String)
    extends SemanticRule(legacyIndex, name) {

  lazy val index: DocumentIndex = {
    val scalafixDoc = legacyIndex.asInstanceOf[DocSemanticdbIndex].doc
    val semanticdbDoc = scalafixDoc.sdoc
    new DocumentIndex(semanticdbDoc)
  }
}

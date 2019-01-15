// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scalafix.internal.v0

case class DocumentIndex(legacyIndex: LegacySemanticdbIndex) {
  lazy val symbols: DocumentSymbols = {
    DocumentSymbols(legacyIndex)
  }
}

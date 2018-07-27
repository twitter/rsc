// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scalafix.v0

import scala.meta.internal.{semanticdb => s}

case class DocumentIndex(val doc: s.TextDocument) {
  lazy val symbols: DocumentSymbols = {
    DocumentSymbols(doc)
  }

  lazy val synthetics: Map[s.Range, s.Synthetic] = {
    doc.synthetics.map(synth => synth.range.get -> synth).toMap
  }

  def withText(text: String): DocumentIndex = {
    DocumentIndex(doc.copy(text = text))
  }
}

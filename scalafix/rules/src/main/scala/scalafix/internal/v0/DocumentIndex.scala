// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scalafix.internal.v0

import scala.meta._
import scala.meta.internal.{semanticdb => s}

case class DocumentIndex(legacyIndex: LegacySemanticdbIndex) {
  lazy val doc: s.TextDocument = {
    legacyIndex.doc.internal.textDocument
  }

  lazy val input: Input = {
    Input.VirtualFile(doc.uri, doc.text)
  }

  def substring(range: Option[s.Range]): Option[String] = {
    range.flatMap { range =>
      if (doc.text.nonEmpty) {
        val pos = Position.Range(
          input,
          range.startLine,
          range.startCharacter,
          range.endLine,
          range.endCharacter)
        Some(pos.text)
      } else {
        None
      }
    }
  }

  lazy val symbols: DocumentSymbols = {
    DocumentSymbols(legacyIndex)
  }

  lazy val synthetics: Map[s.Range, s.Synthetic] = {
    doc.synthetics.map(synth => synth.range.get -> synth).toMap
  }
}

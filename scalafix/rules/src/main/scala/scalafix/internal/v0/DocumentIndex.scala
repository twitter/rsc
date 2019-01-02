// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scalafix.internal.v0

import scala.meta._
import scala.meta.internal.{semanticdb => s}

case class DocumentIndex(val doc: s.TextDocument) {
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
    DocumentSymbols(doc)
  }

  lazy val synthetics: Map[s.Range, s.Synthetic] = {
    doc.synthetics.map(synth => synth.range.get -> synth).toMap
  }

  def withText(text: String): DocumentIndex = {
    DocumentIndex(doc.copy(text = text))
  }
}

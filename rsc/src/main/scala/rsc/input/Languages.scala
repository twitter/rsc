// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.input

sealed trait Language
sealed trait KnownLanguage extends Language
case object ScalaLanguage extends KnownLanguage
case object JavaLanguage extends KnownLanguage
case object UnknownLanguage extends Language

trait Languages {
  implicit class LanguagePathOps(path: java.nio.file.Path) {
    def lang: Language = {
      if (path.toString.endsWith(".scala")) ScalaLanguage
      else if (path.toString.endsWith(".java")) JavaLanguage
      else UnknownLanguage
    }
  }
}

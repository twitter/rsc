// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.inputs

sealed trait Language
sealed trait SupportedLanguage extends Language
case object ScalaLanguage extends SupportedLanguage
case object JavaLanguage extends SupportedLanguage
case object UnsupportedLanguage extends Language

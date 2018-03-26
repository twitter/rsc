// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import scala.meta.internal.semanticdb3._

trait Instances {
  implicit val accStr = Str[Accessibility](PrettySymtabAccessibility.str)
  implicit val accRepl = Repl[Accessibility](PrettySymtabAccessibility.repl)

  implicit val annStr = Str[Annotation](PrettySymtabAnnotation.str)
  implicit val annRepl = Repl[Annotation](PrettySymtabAnnotation.repl)

  implicit val infoStr = Str[SymbolInformation](PrettySymtabInfo.str)
  implicit val infoRepl = Repl[SymbolInformation](PrettySymtabInfo.repl)

  implicit val typeStr = Str[Type](PrettySymtabType.str)
  implicit val typeRepl = Repl[Type](PrettySymtabType.repl)
}

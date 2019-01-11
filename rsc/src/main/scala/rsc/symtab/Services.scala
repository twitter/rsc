// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.symtab

import rsc.semantics._

trait Services {
  self: Symtab =>

  def metadata(sym: Symbol): Metadata = {
    if (outlines.contains(sym)) {
      OutlineMetadata(outlines(sym))
    } else {
      if (classpath.contains(sym)) {
        ClasspathMetadata(classpath(sym))
      } else {
        NoMetadata
      }
    }
  }
}

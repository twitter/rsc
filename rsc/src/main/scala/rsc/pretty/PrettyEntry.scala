// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import rsc.classpath._

object PrettyEntry {
  def str(p: Printer, x: Entry): Unit = {
    x match {
      case PackageEntry() =>
        p.str("<package>")
      case UncompressedEntry(path) =>
        p.str(path.toString)
      case CompressedEntry(jar, entry) =>
        p.str(s"jar:${jar.getName}!${entry.getName}")
    }
  }

  def repl(p: Printer, x: Entry): Unit = {
    ???
  }
}

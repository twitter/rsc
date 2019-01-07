// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.symtab

import rsc.settings._

trait Classpaths extends AutoCloseable {
  def settings: Settings
  val classpath: Classpath = Classpath(settings.cp)
  def close(): Unit = classpath.close()
}

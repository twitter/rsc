// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.checkbase

trait SimpleBase[S <: SettingsBase, N, R] extends MainBase[S, S, N, R] {
  def inputs(settings: S) = List(settings)
}

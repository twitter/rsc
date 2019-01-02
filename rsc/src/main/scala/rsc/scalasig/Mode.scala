// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.scalasig

sealed trait Mode {
  def emitModules: Boolean = this == ToplevelMode || this == ModuleRefMode
  def emitModuleClasses: Boolean = this == RefMode
}
case object ToplevelMode extends Mode
case object RefMode extends Mode
case object ModuleRefMode extends Mode

// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scala.meta.internal.mjar

sealed trait Mode {
  def emitModules: Boolean = this == ToplevelMode || this == ModuleRefMode
  def emitModuleClasses: Boolean = this == RefMode
}
case object ToplevelMode extends Mode
case object RefMode extends Mode
case object ModuleRefMode extends Mode

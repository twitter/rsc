// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.typecheck

import java.io._
import rsc.report._
import rsc.semantics._
import rsc.settings._
import rsc.syntax._
import rsc.util._

final class Linker private (
    settings: Settings,
    reporter: Reporter,
    symtab: Symtab,
    todo: Todo) {
  def apply(trees: List[Source], classpath: List[File]): Unit = {
    val pi = createPi()
    val rootPackage = createPackage(pi, "_root_")
    val emptyPackage = createPackage(pi, "_empty_")
    // NOTE: We expect the deps to be available as stubs on the sourcepath.
    // See stdlib/src/main/scala/Stdlib.scala for an example for such stubs.
  }

  private def createPi(): Uid = {
    val uid = "π."
    val scope = PackageScope(uid)
    symtab.scopes(uid) = scope
    todo.scopes.add(Env(), scope)
    symtab.outlines(uid) = DefnPackage(TermId("π").withUid(uid), Nil)
    uid
  }

  private def createPackage(owner: Uid, value: String): Uid = {
    val sid = TermSid(value)
    val uid = {
      if (owner == "π.") value + "."
      else owner + value + "."
    }
    val outline = DefnPackage(TermId(value).withUid(uid), Nil)
    val scope = PackageScope(uid)
    todo.scopes.add(Env() -> scope)
    val ownerScope = symtab.scopes(owner)
    ownerScope.enter(sid, uid) match {
      case NoUid =>
        symtab.scopes(uid) = scope
        symtab.outlines(uid) = outline
        uid
      case _ =>
        unreachable(ownerScope)
    }
  }
}

object Linker {
  def apply(
      settings: Settings,
      reporter: Reporter,
      symtab: Symtab,
      todo: Todo): Linker = {
    new Linker(settings, reporter, symtab, todo)
  }
}

// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.outline

import rsc.classpath._
import rsc.input._
import rsc.report._
import rsc.semantics._
import rsc.settings._
import rsc.symtab._
import rsc.util._

final class Indexer private (
    settings: Settings,
    reporter: Reporter,
    classpath: Classpath,
    symtab: Symtab,
    todo: Todo) {
  def apply(): Unit = {
    val rootScope = PackageScope(RootPackage, classpath)
    symtab.scopes.put(rootScope.sym, rootScope)
    todo.add(Env(Nil, ScalaLanguage), rootScope)
    val emptyScope = PackageScope(EmptyPackage, classpath)
    symtab.scopes.put(emptyScope.sym, emptyScope)
    todo.add(Env(Nil, ScalaLanguage), emptyScope)

    sanityCheck("java/lang/", JavaLanguage, ScalaLanguage)
    sanityCheck("scala/", ScalaLanguage)
    sanityCheck("scala/Predef.", ScalaLanguage)
    sanityCheck("scala/AnyRef#", ScalaLanguage)
  }

  private def sanityCheck(fundamentalSym: String, symbolLangs: Language*): Unit = {
    val programLangs = settings.ins.map(path => Input(path).lang).toSet
    if (symbolLangs.exists(programLangs.contains)) {
      if (!symtab.scopes.contains(fundamentalSym)) {
        crash(s"""
        |missing fundamental symbol: $fundamentalSym
        |Unlike Javac and Scalac, Rsc does not automatically prepopulate its classpath.
        |Please pass the following libraries explicitly:
        |  1) JDK libraries that are used in your code.
        |  2) (If compiling Scala code) Scala library
        |  3) (If compiling Scala code) Scala library synthetics, i.e. a SemanticDB-only
        |     artifact produced by the Metacp tool from the Scalameta toolchain.
        """.trim.stripMargin)
      }
    }
  }
}

object Indexer {
  def apply(
      settings: Settings,
      reporter: Reporter,
      classpath: Classpath,
      symtab: Symtab,
      todo: Todo): Indexer = {
    new Indexer(settings, reporter, classpath, symtab, todo)
  }
}

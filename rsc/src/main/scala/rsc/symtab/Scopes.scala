// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.symtab

import scala.collection.mutable
import scala.meta.internal.semanticdb3._
import scala.meta.internal.{semanticdb3 => s}
import scala.meta.internal.semanticdb3.Scala._
import scala.meta.internal.semanticdb3.SymbolInformation.{Kind => k}
import scala.meta.internal.semanticdb3.Type.{Tag => t}
import rsc.pretty._
import rsc.semantics._
import rsc.util._

sealed abstract class Scope(val sym: Symbol) extends Pretty {
  def lookup(name: Name): Symbol
  def members: Iterator[Symbol]
  override def printStr(p: Printer): Unit = PrettySymtabScope.str(p, this)
  override def printRepl(p: Printer): Unit = PrettySymtabScope.repl(p, this)
}

final class PackageScope private (sym: Symbol) extends Scope(sym) {
  val _storage = new mutable.LinkedHashMap[Name, Symbol]

  override def lookup(name: Name): Symbol = {
    _storage.getOrElse(name, NoSymbol)
  }

  override def members: Iterator[Symbol] = {
    _storage.valuesIterator
  }
}

object PackageScope {
  def apply(sym: Symbol) = {
    new PackageScope(sym)
  }
}

final class TemplateScope private (
    val info: SymbolInformation,
    val symtab: Symtab)
    extends Scope(info.symbol) {
  var _storage: mutable.LinkedHashMap[Name, Symbol] = null
  var _parents: mutable.UnrolledBuffer[TemplateScope] = null

  private def loadStorage(): Unit = {
    info.tpe.flatMap(_.classInfoType) match {
      case Some(ClassInfoType(_, _, decls)) =>
        _storage = mutable.LinkedHashMap[Name, Symbol]()
        decls.foreach(decl => _storage(Name(decl.desc.toString)) = decl)
      case other =>
        crash(info)
    }
  }

  private def loadParents(): Unit = {
    info.tpe.flatMap(_.classInfoType) match {
      case Some(ClassInfoType(_, parents, _)) =>
        def parentScope(tpe: s.Type): TemplateScope = {
          if (tpe.tag == t.TYPE_REF) {
            val Some(TypeRef(_, sym, targs)) = tpe.typeRef
            val info = symtab.infos(sym)
            info.kind match {
              case k.OBJECT | k.PACKAGE_OBJECT | k.CLASS | k.TRAIT |
                  k.INTERFACE =>
                TemplateScope(info, symtab)
              case k.TYPE =>
                val hi = info.tpe.flatMap(_.typeType.flatMap(_.upperBound)).get
                parentScope(hi)
              case _ =>
                crash(info)
            }
          } else {
            crash(tpe)
          }
        }
        _parents = mutable.UnrolledBuffer[TemplateScope]()
        parents.map(parentScope).foreach(_parents += _)
      case other =>
        crash(info)
    }
  }

  override def lookup(name: Name): Symbol = {
    if (_storage == null) loadStorage()
    _storage.getOrElse(name, {
      if (_parents == null) loadParents()
      var result: Symbol = NoSymbol
      _parents.foreach(p => if (result == NoSymbol) result = p.lookup(name))
      result
    })
  }

  override def members: Iterator[Symbol] = {
    if (_storage == null) loadStorage()
    if (_parents == null) loadParents()
    val decls = _storage.valuesIterator
    val inherited = _parents.iterator.flatMap(_.members)
    decls ++ inherited
  }
}

object TemplateScope {
  def apply(info: SymbolInformation, symtab: Symtab) = {
    new TemplateScope(info, symtab)
  }
}

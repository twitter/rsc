// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.rules.semantics

import scala.collection.mutable
import scala.meta._
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.Scala.{Descriptor => d}
import scala.meta.internal.semanticdb.Scala.{Names => n}

sealed trait Scope {
  def lookup(name: n.Name): String

  protected def member(symtab: Symtab, sym: String, name: n.Name): String = {
    def getInfo(desc: Descriptor): Option[s.SymbolInformation] =
      symtab
        .info(Symbols.Global(sym, desc))
        .orElse(symtab.info(Symbols.Global(s"${sym}package.", desc)))

    val info = name match {
      case n.TermName(value) => getInfo(d.Package(value)).orElse(getInfo(d.Term(value)))
      case n.TypeName(value) => getInfo(d.Type(value))
    }
    info.map(_.symbol).getOrElse(Symbols.None)
  }
}

final class AddedImportsScope extends Scope {

  def lookup(name: n.Name): String = addedNames.getOrElse(name, "")

  def addImport(sym: String): Unit = {
    if (!addedImports.contains(sym)) {
      val fqn = sym.init.replace('/', '.').replace("package.", "")

      fqn.parse[Importer].toOption.foreach { importer =>
        val name = sym.desc.name
        addedNames += (name -> sym)
        addedImports += (sym -> importer)
      }
    }
  }

  def importers: Seq[Importer] = addedImports.values.toSeq

  private var addedNames = mutable.Map.empty[n.Name, String]

  private var addedImports = mutable.Map.empty[String, Importer]
}

final case class ImporterScope(symtab: Symtab, sym: String, importees: List[Importee])
    extends Scope {
  private val mappings = mutable.Map[String, Option[String]]()
  private var wildcard = false
  importees.foreach {
    case Importee.Wildcard() => wildcard = true
    case Importee.Name(name) => mappings.put(name.value, Some(name.value))
    case Importee.Rename(name, rename) => mappings.put(name.value, Some(rename.value))
    case Importee.Unimport(name) => mappings.put(name.value, None)
  }

  def lookup(name: n.Name): String = {
    val value1 = {
      val value1 = mappings.get(name.value)
      if (wildcard && value1.isEmpty) Some(name.value)
      else value1.flatten
    }
    val name1 = name match {
      case _: n.TermName => value1.map(n.TermName)
      case _: n.TypeName => value1.map(n.TypeName)
    }
    name1.map(member(symtab, sym, _)).getOrElse(Symbols.None)
  }

  override def toString: String = {
    s"import $sym ${importees.mkString(",")}"
  }
}

final case class PackageScope(symtab: Symtab, sym: String) extends Scope {
  def lookup(name: n.Name): String = {
    member(symtab, sym, name)
  }

  override def toString: String = {
    s"package $sym"
  }
}

final case class TemplateScope(symtab: Symtab, sym: String) extends Scope {
  def lookup(name: n.Name): String = {
    linearization.foldLeft("") { (acc, symbol) =>
      if (acc.isEmpty) member(symtab, symbol, name) else acc
    }
  }

  override def toString: String = {
    s"template $sym"
  }

  private val linearization = linearize(sym)

  private def linearize(symbol: String): List[String] = {
    def replaceAndConcat(xs: List[String], ys: List[String]) = xs.filterNot(ys.contains) ++ ys

    // FIXME: https://github.com/twitter/rsc/issues/318
    val parents = classSignature(symbol).parents

    val parentSymbols = parents.collect { case s.TypeRef(_, symbol, _) => symbol }

    parentSymbols.foldRight(List(symbol)) { (parent, acc) =>
      replaceAndConcat(acc, linearize(parent))
    }
  }

  private def classSignature(symbol: String): s.ClassSignature =
    symtab.info(symbol).get.signature match {
      case x: s.ClassSignature => x

      case s.TypeSignature(_, s.TypeRef(_, lo, _), s.TypeRef(_, hi, _)) if lo == hi =>
        classSignature(lo)
    }
}

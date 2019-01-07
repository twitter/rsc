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
    val info = name match {
      case n.TermName(value) =>
        val packageMember = symtab.info(Symbols.Global(sym, d.Package(value)))
        val termMember = symtab.info(Symbols.Global(sym, d.Term(value)))
        packageMember.orElse(termMember)
      case n.TypeName(value) =>
        symtab.info(Symbols.Global(sym, d.Type(value)))
    }
    info.map(_.symbol).getOrElse(Symbols.None)
  }
}

final class AddedImportsScope extends Scope {

  def lookup(name: n.Name): String = addedNames.getOrElse(name, "")

  def addImport(sym: String): Unit = {
    if (!addedImports.contains(sym)) {
      sym.init.replace('/', '.').parse[Importer].toOption.foreach { importer =>
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
    val s.ClassSignature(_, parents, self, _) = symtab.info(sym).get.signature
    // TODO: Also lookup in parents and self.
    member(symtab, sym, name)
  }

  override def toString: String = {
    s"template $sym"
  }
}

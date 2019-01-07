// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.outline

import java.util.{LinkedHashMap, Map}
import rsc.classpath._
import rsc.semantics._
import rsc.syntax._
import rsc.util._
import scala.collection.JavaConverters._
import scala.meta.internal.semanticdb.Scala.{Descriptor => d}

sealed abstract class Scope(val sym: Symbol) extends Work {
  def enter(name: Name, sym: Symbol): Symbol

  def resolve(name: Name): SymbolResolution = {
    status match {
      case PendingStatus =>
        BlockedResolution(this)
      case BlockedStatus(_) =>
        BlockedResolution(this)
      case _: FailedStatus =>
        ErrorResolution
      case SucceededStatus =>
        crash(this)
    }
  }
}

// ============ TWO FOUNDATIONAL SCOPES ============

sealed trait BinaryScope extends Scope {
  def index: Index

  private val loaded: Map[Name, Symbol] = new LinkedHashMap[Name, Symbol]
  protected def load(name: Name): Symbol = {
    val loadedSym = loaded.get(name)
    if (loadedSym != null) {
      loadedSym
    } else {
      val loadedSym = loadMember(sym, name)
      loaded.put(name, loadedSym)
      loadedSym
    }
  }

  private def loadMember(owner: Symbol, name: Name): Symbol = {
    val declSym = loadDecl(owner, name)
    if (declSym != NoSymbol) {
      return declSym
    }

    val info = index(owner)

    // FIXME: https://github.com/twitter/rsc/issues/229.
    // Utilizing selfs is probably incorrect when doing lookups from Java,
    // but hopefully we'll rewrite the name resolution logic before this becomes a problem.
    (info.parents ++ info.self).foreach { parent =>
      val memberSym = loadMember(parent, name)
      if (memberSym != NoSymbol) {
        return memberSym
      }
    }

    // FIXME: https://github.com/twitter/rsc/issues/229.
    // Utilizing package objects is incorrect when doing lookups from Java,
    // but hopefully we'll rewrite the name resolution logic before this becomes a problem.
    if (info.isPackage) {
      val packageObjectSym = TermSymbol(owner, "package")
      if (index.contains(packageObjectSym)) {
        val packageObjectMemberSym = loadMember(packageObjectSym, name)
        if (index.contains(packageObjectMemberSym)) {
          return packageObjectMemberSym
        }
      }
    }

    NoSymbol
  }

  private def loadDecl(owner: Symbol, name: Name): Symbol = {
    val declSym = {
      name match {
        case TermName(value) =>
          TermSymbol(owner, value)
        case TypeName(value) =>
          TypeSymbol(owner, value)
      }
    }
    if (index.contains(declSym)) {
      return declSym
    }

    name match {
      case TermName(value) =>
        val packageSym = PackageSymbol(owner, value)
        if (index.contains(packageSym)) {
          return packageSym
        }

        // FIXME: https://github.com/twitter/rsc/issues/229.
        // This is accidentally correct when doing lookups from Java,
        // because Java programs don't have TermIds in reference roles.
        val javaDeclSym = TypeSymbol(owner, value)
        if (index.contains(javaDeclSym) && index(javaDeclSym).isJava) {
          return javaDeclSym
        }
      case _ =>
        ()
    }

    name match {
      case TypeName(value) if value.endsWith("$") =>
        val moduleSym = loadDecl(owner, TermName(value.stripSuffix("$")))
        if (index.contains(moduleSym)) {
          return moduleSym
        }
      case _ =>
        ()
    }

    NoSymbol
  }
}

sealed abstract class SourceScope(sym: Symbol) extends Scope(sym) {
  private val impl: Map[Name, Symbol] = new LinkedHashMap[Name, Symbol]

  def decls: List[Symbol] = {
    impl.values.asScala.toList
  }

  override def enter(name: Name, sym: Symbol): Symbol = {
    if (status.isPending) {
      sym match {
        case NoSymbol =>
          crash(name)
        case _ =>
          val existing = impl.get(name)
          if (existing != null) {
            val actual = {
              (existing.desc, sym.desc) match {
                case (_: d.Package, d.Term("package")) => existing
                case (d.Term("package"), _: d.Package) => sym
                case _ => MultiSymbol(existing, sym)
              }
            }
            impl.put(name, actual)
            existing
          } else {
            impl.put(name, sym)
            NoSymbol
          }
      }
    } else {
      crash(this)
    }
  }

  override def resolve(name: Name): SymbolResolution = {
    if (status.isSucceeded) {
      val result = impl.get(name)
      if (result != null) {
        ResolvedSymbol(result)
      } else {
        MissingResolution
      }
    } else {
      super.resolve(name)
    }
  }
}

// ============ BINARY SCOPES ============

final class ClasspathScope private (sym: Symbol, val index: Index)
    extends Scope(sym)
    with BinaryScope {
  override def enter(name: Name, sym: Symbol): Symbol = {
    crash(this)
  }

  override def resolve(name: Name): SymbolResolution = {
    load(name) match {
      case NoSymbol =>
        MissingResolution
      case sym =>
        ResolvedSymbol(sym)
    }
  }
}

object ClasspathScope {
  def apply(sym: Symbol, index: Index): BinaryScope = {
    new ClasspathScope(sym, index)
  }
}

final class PackageScope private (sym: Symbol, val index: Index)
    extends SourceScope(sym)
    with BinaryScope {
  override def resolve(name: Name): SymbolResolution = {
    super.resolve(name) match {
      case MissingResolution =>
        if (index.contains(sym)) {
          val loadedSym = load(name)
          loadedSym match {
            case NoSymbol =>
              MissingResolution
            case loadedSym =>
              ResolvedSymbol(loadedSym)
          }
        } else {
          MissingResolution
        }
      case resolution =>
        resolution
    }
  }
}

object PackageScope {
  def apply(sym: Symbol, index: Index): PackageScope = {
    new PackageScope(sym, index)
  }
}

// ============ SOURCE SCOPES ============

final class ImporterScope private (val tree: Importer) extends Scope(NoSymbol) {
  var _parent1: Scope = null
  var _parent2: Scope = null

  def parent1: Scope = {
    if (status.isSucceeded) {
      _parent1
    } else {
      crash(this)
    }
  }

  def parent1_=(parent1: Scope): Unit = {
    if (status.isPending) {
      _parent1 = parent1
    } else {
      crash(this)
    }
  }

  def parent2: Scope = {
    if (status.isSucceeded) {
      _parent2
    } else {
      crash(this)
    }
  }

  def parent2_=(parent2: Scope): Unit = {
    if (status.isPending) {
      _parent2 = parent2
    } else {
      crash(this)
    }
  }

  override def enter(name: Name, sym: Symbol): Symbol = {
    crash(this)
  }

  private def remap(name: Name): Name = {
    def loop(importees: List[Importee]): String = {
      importees match {
        case ImporteeName(AmbigId(value)) :: rest =>
          if (name.value == value) value
          else loop(rest)
        case ImporteeRename(AmbigId(from), AmbigId(to)) :: rest =>
          if (name.value == from) null
          else if (name.value == to) from
          else loop(rest)
        case ImporteeUnimport(AmbigId(value)) :: rest =>
          if (name.value == value) null
          else loop(rest)
        case ImporteeWildcard() :: rest =>
          name.value
        case Nil =>
          null
      }
    }
    val value1 = loop(tree.importees)
    if (value1 != null) {
      name match {
        case _: TermName =>
          TermName(value1)
        case _: TypeName =>
          TypeName(value1)
      }
    } else {
      null
    }
  }

  override def resolve(name: Name): SymbolResolution = {
    val name1 = remap(name)
    if (name1 != null) {
      status match {
        case PendingStatus =>
          super.resolve(name)
        case BlockedStatus(dep) =>
          super.resolve(name)
        case _: FailedStatus =>
          MissingResolution
        case SucceededStatus =>
          val resolution1 = parent1.resolve(name1)
          resolution1 match {
            case _: BlockedResolution =>
              resolution1
            case _: FailedResolution =>
              if (parent2 != null) {
                parent2.resolve(name1)
              } else {
                resolution1
              }
            case _: ResolvedSymbol =>
              resolution1
          }
      }
    } else {
      MissingResolution
    }
  }

  override def succeed(): Unit = {
    if (_parent1 == null) {
      crash(this)
    }
    super.succeed()
  }
}

object ImporterScope {
  def apply(tree: Importer): ImporterScope = {
    new ImporterScope(tree)
  }
}

final class PackageObjectScope private (
    sym: Symbol,
    tree: DefnPackageObject,
    packageScope: PackageScope)
    extends TemplateScope(sym, tree) {
  override def enter(name: Name, sym: Symbol): Symbol = {
    val existingSym = super.enter(name, sym)
    packageScope.enter(name, sym)
    existingSym
  }
}

object PackageObjectScope {
  def apply(tree: DefnPackageObject, packageScope: PackageScope): PackageObjectScope = {
    new PackageObjectScope(tree.id.sym, tree, packageScope)
  }
}

final class ParamScope private (owner: Symbol) extends SourceScope(owner)

object ParamScope {
  def apply(owner: Symbol): ParamScope = {
    new ParamScope(owner)
  }
}

final class SelfScope private (owner: Symbol) extends SourceScope(owner)

object SelfScope {
  def apply(owner: Symbol): SelfScope = {
    new SelfScope(owner)
  }
}

class TemplateScope protected (sym: Symbol, val tree: DefnTemplate) extends SourceScope(sym) {
  var _parents: List[Scope] = null
  var _self: List[Scope] = null
  var _env: Env = null

  def parents: List[Scope] = {
    if (status.isSucceeded) {
      _parents
    } else {
      crash(this)
    }
  }

  def parents_=(parents: List[Scope]): Unit = {
    if (status.isPending) {
      _parents = parents
      recomputeEnv()
    } else {
      crash(this)
    }
  }

  def self: List[Scope] = {
    if (status.isSucceeded) {
      _self
    } else {
      crash(this)
    }
  }

  def self_=(self: List[Scope]): Unit = {
    if (status.isPending) {
      _self = self
      recomputeEnv()
    } else {
      crash(this)
    }
  }

  private def recomputeEnv(): Unit = {
    if (_parents != null && _self != null) {
      _env = Env(_self ++ _parents, tree.lang)
    } else if (_parents != null && _self == null) {
      _env = Env(_parents, tree.lang)
    } else if (_parents == null && _self != null) {
      _env = Env(_self, tree.lang)
    } else {
      _env = Env(Nil, tree.lang)
    }
  }

  override def resolve(name: Name): SymbolResolution = {
    super.resolve(name) match {
      case MissingResolution =>
        _env.resolve(name)
      case resolution =>
        resolution
    }
  }

  override def succeed(): Unit = {
    if (_parents == null) {
      crash(this)
    }
    super.succeed()
  }
}

object TemplateScope {
  def apply(tree: DefnTemplate): TemplateScope = {
    new TemplateScope(tree.id.sym, tree)
  }
}

final class TypeParamScope private (owner: Symbol) extends SourceScope(owner)

object TypeParamScope {
  def apply(owner: Symbol): TypeParamScope = {
    new TypeParamScope(owner)
  }
}

final class ExistentialScope private () extends SourceScope(NoSymbol)

object ExistentialScope {
  def apply(): ExistentialScope = {
    new ExistentialScope()
  }
}

final class RefineScope private () extends SourceScope(NoSymbol)

object RefineScope {
  def apply(): RefineScope = {
    new RefineScope()
  }
}

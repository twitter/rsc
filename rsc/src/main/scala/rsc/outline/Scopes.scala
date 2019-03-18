// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.outline

import java.util.{LinkedHashMap, Map}
import rsc.classpath._
import rsc.input._
import rsc.semantics._
import rsc.syntax._
import rsc.util._
import scala.collection.JavaConverters._
import scala.meta.internal.{semanticdb => s}
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

sealed trait ClasspathScope extends Scope {
  protected def classpath: Classpath

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

    val info = classpath(owner)

    info.parents.foreach { parent =>
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
      if (classpath.contains(packageObjectSym)) {
        val packageObjectMemberSym = loadMember(packageObjectSym, name)
        if (classpath.contains(packageObjectMemberSym)) {
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
    if (classpath.contains(declSym)) {
      return declSym
    }

    name match {
      case TermName(value) =>
        val packageSym = PackageSymbol(owner, value)
        if (classpath.contains(packageSym)) {
          return packageSym
        }

        // FIXME: https://github.com/twitter/rsc/issues/229.
        // This is quite incorrect, but quite convenient.
        val methodSym = MethodSymbol(owner, value, "()")
        if (classpath.contains(methodSym)) {
          return methodSym
        }

        // FIXME: https://github.com/twitter/rsc/issues/229.
        // This is accidentally correct when doing lookups from Java,
        // because Java programs don't have TermIds in reference roles.
        val javaDeclSym = TypeSymbol(owner, value)
        if (classpath.contains(javaDeclSym) && classpath(javaDeclSym).isJava) {
          return javaDeclSym
        }
      case _ =>
        ()
    }

    name match {
      case TypeName(value) if value.endsWith("$") =>
        val moduleSym = loadDecl(owner, TermName(value.stripSuffix("$")))
        if (classpath.contains(moduleSym)) {
          return moduleSym
        }
      case _ =>
        ()
    }

    NoSymbol
  }
}

sealed abstract class OutlineScope(sym: Symbol) extends Scope(sym) {
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

// ============ CLASSPATH SCOPES ============

final class PackageScope private (sym: Symbol, protected val classpath: Classpath)
    extends OutlineScope(sym)
    with ClasspathScope {
  override def resolve(name: Name): SymbolResolution = {
    super.resolve(name) match {
      case MissingResolution =>
        if (classpath.contains(sym)) {
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
  def apply(sym: Symbol, classpath: Classpath): PackageScope = {
    new PackageScope(sym, classpath)
  }
}

final class SignatureScope private (sym: Symbol, protected val classpath: Classpath)
    extends Scope(sym)
    with ClasspathScope {
  def signature: s.ClassSignature = {
    classpath(sym).signature.asInstanceOf[s.ClassSignature]
  }

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

object SignatureScope {
  def apply(sym: Symbol, classpath: Classpath): ClasspathScope = {
    new SignatureScope(sym, classpath)
  }
}

// ============ OUTLINE SCOPES ============

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

  private def remap(name: Name): (Name, Boolean) = {
    var wildcard = false
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
          wildcard = true
          name.value
        case Nil =>
          null
      }
    }
    val value1 = loop(tree.importees)
    val name1 = {
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
    (name1, wildcard)
  }

  override def resolve(name: Name): SymbolResolution = {
    val (name1, wildcard) = remap(name)
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
            case ResolvedSymbol(sym) =>
              if (wildcard) {
                WildcardSymbol(sym)
              } else {
                ExplicitSymbol(sym)
              }
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

final class PackageObjectScope private (tree: DefnPackageObject, packageScope: PackageScope)
    extends TemplateScope(tree) {
  override def enter(name: Name, sym: Symbol): Symbol = {
    val existingSym = super.enter(name, sym)
    packageScope.enter(name, sym)
    existingSym
  }
}

object PackageObjectScope {
  def apply(tree: DefnPackageObject, packageScope: PackageScope): PackageObjectScope = {
    new PackageObjectScope(tree, packageScope)
  }
}

final class ParamScope private (owner: Symbol) extends OutlineScope(owner)

object ParamScope {
  def apply(owner: Symbol): ParamScope = {
    new ParamScope(owner)
  }
}

final class SelfScope private (val owner: DefnTemplate) extends OutlineScope(owner.id.sym) {
  val tree: Self = owner.self.get
  var _parent: Scope = null

  def parent: Scope = {
    if (status.isSucceeded) {
      _parent
    } else {
      crash(this)
    }
  }

  def parent_=(parent: Scope): Unit = {
    if (status.isPending) {
      _parent = parent
    } else {
      crash(this)
    }
  }

  override def resolve(name: Name): SymbolResolution = {
    super.resolve(name) match {
      case MissingResolution =>
        if (_parent != null) {
          _parent.resolve(name)
        } else {
          MissingResolution
        }
      case resolution =>
        resolution
    }
  }

  override def succeed(): Unit = {
    super.succeed()
  }
}

object SelfScope {
  def apply(owner: DefnTemplate): SelfScope = {
    new SelfScope(owner)
  }
}

class TemplateScope protected (val tree: DefnTemplate) extends OutlineScope(tree.id.sym) {
  // FIXME: https://github.com/twitter/rsc/issues/229
  // This shouldn't be modelled as Env since Env.resolve and linearization are two different things.
  private var _parents: List[Scope] = null
  private var _env: Env = null

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
      _env = Env(Root(tree.lang), _parents)
    } else {
      crash(this)
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
    new TemplateScope(tree)
  }
}

final class TypeParamScope private (owner: Symbol) extends OutlineScope(owner)

object TypeParamScope {
  def apply(owner: Symbol): TypeParamScope = {
    new TypeParamScope(owner)
  }
}

final class ExistentialScope private () extends OutlineScope(NoSymbol)

object ExistentialScope {
  def apply(): ExistentialScope = {
    new ExistentialScope()
  }
}

final class RefineScope private () extends OutlineScope(NoSymbol)

object RefineScope {
  def apply(): RefineScope = {
    new RefineScope()
  }
}

final class WithScope private (val parents: List[Scope]) extends Scope(NoSymbol) {
  // FIXME: https://github.com/twitter/rsc/issues/229
  // This shouldn't be modelled as Env since Env.resolve and linearization are two different things.
  var _env: Env = Env(Root(ScalaLanguage), parents)

  override def enter(name: Name, sym: Symbol): Symbol = {
    crash(this)
  }

  override def resolve(name: Name): SymbolResolution = {
    if (status.isSucceeded) {
      _env.resolve(name)
    } else {
      super.resolve(name)
    }
  }
}

object WithScope {
  def apply(scopes: List[Scope]): WithScope = {
    new WithScope(scopes)
  }
}

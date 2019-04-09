// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.semanticdb

import rsc.outline._
import rsc.semantics._
import rsc.syntax._
import rsc.util._
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.Scala.{Descriptor => d}

trait Prefixes {
  self: Converter =>

  private lazy val env: Env = {
    symtab.envs(root.id.sym)
  }

  def prefix(id: Id): s.Type = {
    if (id.sym.isRootPackage) {
      s.NoType
    } else if (id.sym.isEmptyPackage) {
      s.NoType
    } else if (id.sym.desc.isParameter || id.sym.desc.isTypeParameter) {
      s.NoType
    } else {
      val ownerSym = id.sym.owner
      ownerSym.desc match {
        case d.Type(value) =>
          env.resolveThis(value) match {
            // We need to check the companion object because
            // despite metac generating semanticdb with a prefix in the case of a companion object,
            // metacp does not contain a prefix: see example file 417c.scala
            case ResolvedSymbol(sym) if sym == ownerSym || sym == ownerSym.companionObject =>
              s.NoType
            case ResolvedSymbol(_) | MissingResolution =>
              // FIXME: https://github.com/twitter/rsc/issues/229
              def loop(scopes: List[Scope]): s.Type = {
                // TODO: should we refactor this resolution logic into Env?
                // We would need to return the relevant scope as well as the SymbolResolution
                scopes match {
                  case head :: tail =>
                    val resolution = {
                      id match {
                        case id: AmbigId =>
                          head.resolve(TermName(id.value)) match {
                            case resolved: ResolvedSymbol => resolved
                            case other => head.resolve(TypeName(id.value))
                          }
                        case id: AnonId =>
                          crash(id)
                        case id: NamedId =>
                          head.resolve(id.name)
                      }
                    }
                    resolution match {
                      case _: ResolvedSymbol =>
                        head match {
                          case head: ImporterScope =>
                            prefix(head.tree.qual, id)
                          case head: TemplateScope =>
                            val thisId = AmbigId(head.tree.id.value).withSym(head.tree.id.sym)
                            prefix(TermThis(thisId), id)
                          case head: SelfScope =>
                            val thisId = AmbigId(head.owner.id.value).withSym(head.owner.id.sym)
                            prefix(TermThis(thisId), id)
                          case other =>
                            crash(other)
                        }
                      case MissingResolution =>
                        loop(tail)
                      case other =>
                        crash(other)
                    }
                  case Nil =>
                    crash(id)
                }
              }
              loop(env.scopes)
            case other =>
              crash(other)
          }
        case _ =>
          s.NoType
      }
    }
  }

  def prefix(qual: Path, id: Id): s.Type = {
    val needsPrefix = {
      qual match {
        case qual: TptPath =>
          true
        case _ =>
          val qualSym = {
            val outline = symtab.outlines.get(qual.id.sym)
            outline match {
              // FIXME: https://github.com/twitter/rsc/issues/261
              // FIXME: https://github.com/scalameta/scalameta/issues/1808
              case Some(_: Self) => qual.id.sym.stripPrefix("local").stripSuffix("=>")
              case _ => qual.id.sym
            }
          }
          val ownerSym = id.sym.owner
          if (qualSym != ownerSym) {
            ownerSym.desc match {
              case d.Term("package") =>
                qualSym != ownerSym.owner
              case _ =>
                symtab.metadata(id.sym) match {
                  case OutlineMetadata(outline) => !outline.hasStatic
                  case ClasspathMetadata(info) => !info.isStatic
                  case NoMetadata => false
                }
            }
          } else {
            false
          }
      }
    }
    if (needsPrefix) {
      qual match {
        case id: AmbigId => s.TypeRef(prefix(id), id.sym, Nil)
        case AmbigSelect(qual, id) => s.TypeRef(prefix(qual, id), id.sym, Nil)
        case qual: TermPath => TptSingleton(qual).tpe
        case qual: TptPath => qual.tpe
        case other => crash(other)
      }
    } else {
      s.NoType
    }
  }
}

// Copyright (c) 2017-2018 Twitter, Inc.
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
    val env = symtab._envs.get(root.id.sym)
    if (env == null) crash(root)
    env
  }

  def prefix(id: Id): s.Type = {
    if (id.sym.isRootPackage) {
      s.NoType
    } else if (id.sym.isEmptyPackage) {
      s.NoType
    } else if (id.sym.desc.isParameter || id.sym.desc.isTypeParameter) {
      s.NoType
    } else {
      id.sym.owner.desc match {
        case d.Type(value) =>
          env.resolveThis(value) match {
            case FoundResolution(sym) =>
              s.NoType
            case MissingResolution =>
              // FIXME: https://github.com/twitter/rsc/issues/229
              def loop(scopes: List[Scope]): s.Type = {
                scopes match {
                  case head :: tail =>
                    val resolution = {
                      id match {
                        case id: AmbigId =>
                          head.resolve(TermName(id.value)) match {
                            case found: FoundResolution => found
                            case other => head.resolve(TypeName(id.value))
                          }
                        case id: AnonId =>
                          crash(id)
                        case id: NamedId =>
                          head.resolve(id.name)
                      }
                    }
                    resolution match {
                      case _: FoundResolution =>
                        head match {
                          case head: ImporterScope =>
                            prefix(head.tree.qual, id)
                          case head: TemplateScope =>
                            val thisId = AmbigId(head.tree.id.value).withSym(head.tree.id.sym)
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
              loop(env._scopes)
            case other =>
              crash(other)
          }
        case _ =>
          s.NoType
      }
    }
  }

  // NOTE: In this method, qual can either be TermPath (for Scala) or AmbigPath (for Java).
  // I briefly considered introducing QualPath that would be a common supertype for these traits,
  // but that sounded overly specific and was incompatible with existing tree fields called qual.
  def prefix(qual: Path, id: Id): s.Type = {
    val needsPrefix = {
      if (id.sym.owner != qual.id.sym) {
        id.sym.owner.desc match {
          case d.Term("package") =>
            id.sym.owner.owner != qual.id.sym
          case _ =>
            val outline = symtab._outlines.get(id.sym)
            if (outline != null) {
              !outline.hasStatic
            } else {
              if (symtab._index.contains(id.sym)) {
                val info = symtab._index(id.sym)
                !info.isStatic
              } else {
                crash(id.sym)
              }
            }
        }
      } else {
        false
      }
    }
    if (needsPrefix) {
      qual match {
        case id: AmbigId => s.TypeRef(prefix(id), id.sym, Nil)
        case AmbigSelect(qual, id) => s.TypeRef(prefix(qual, id), id.sym, Nil)
        case qual: TermPath => TptSingleton(qual).tpe
        case other => crash(other)
      }
    } else {
      s.NoType
    }
  }
}

// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.outline

import rsc.pretty._
import rsc.syntax._

// FIXME: https://github.com/twitter/rsc/issues/104

sealed trait Atom extends Pretty with Product {
  def printStr(p: Printer): Unit = PrettyAtom.str(p, this)
  def printRepl(p: Printer): Unit = PrettyAtom.repl(p, this)
}

final case class AmbigAtom(id: AmbigId) extends Atom

final case class NamedAtom(id: NamedId) extends Atom

final case class SuperAtom(id: SuperId) extends Atom

final case class ThisAtom(id: ThisId) extends Atom

final case class UnsupportedAtom(unsupported: Tree) extends Atom

trait Atoms {
  implicit class PathAtomsOps(path: Path) {
    def atoms: List[Atom] = {
      path match {
        case id: AmbigId => List(AmbigAtom(id))
        case AmbigSelect(qual, id) => qual.atoms ++ id.atoms
        case id: NamedId => List(NamedAtom(id))
        case TermSelect(qual: Path, id) => qual.atoms ++ id.atoms
        case TermSelect(qual, id) => List(UnsupportedAtom(qual)) ++ id.atoms
        case TermSuper(qual, mix) => List(ThisAtom(qual), SuperAtom(mix))
        case TermThis(qual) => List(ThisAtom(qual))
        case TptProject(qual, id) => List(UnsupportedAtom(qual)) ++ id.atoms
        case TptSelect(qual, id) => qual.atoms ++ id.atoms
        case TptSingleton(qual) => qual.atoms
      }
    }
  }
}

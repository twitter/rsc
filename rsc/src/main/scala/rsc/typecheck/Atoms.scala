// Copyright (c) 2017 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.typecheck

import rsc.pretty._
import rsc.syntax._

sealed trait Atom extends Pretty with Product {
  def printStr(p: Printer): Unit = PrettyAtom.str(p, this)
  def printRepl(p: Printer): Unit = PrettyAtom.repl(p, this)
}

final case class ApplyAtom(args: List[Tpt]) extends Atom

final case class IdAtom(id: NamedId) extends Atom

final case class SuperAtom(id: Id) extends Atom

final case class ThisAtom(id: Id) extends Atom

final case class UnsupportedAtom(unsupported: Tree) extends Atom

trait Atoms {
  implicit class PathAtomsOps(path: Path) {
    def atoms: List[Atom] = {
      path match {
        case id: NamedId => List(IdAtom(id))
        case TermSelect(qual: Path, id) => qual.atoms ++ id.atoms
        case TermSelect(qual, id) => List(UnsupportedAtom(qual)) ++ id.atoms
        case TermSuper(qual, mix) => List(ThisAtom(qual), SuperAtom(mix))
        case TermThis(qual) => List(ThisAtom(qual))
        case TptSelect(qual, id) => qual.atoms ++ id.atoms
      }
    }
  }

  implicit class TptAtomsOps(tpt: Tpt) {
    def atoms: List[Atom] = {
      tpt match {
        case TptApply(tpt, args) => tpt.atoms ++ List(ApplyAtom(args))
        case tpt: TptPath => PathAtomsOps(tpt).atoms
      }
    }
  }

  implicit class TptPathAtomsOps(tptPath: TptPath) {
    def atoms: List[Atom] = {
      PathAtomsOps(tptPath).atoms
    }
  }
}

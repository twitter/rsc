// Copyright (c) 2017 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.typecheck

import rsc.lexis._
import rsc.pretty._
import rsc.syntax._

sealed trait Atom extends Pretty with Product {
  var pos: Position = NoPosition
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
        case id: NamedId =>
          val atom = IdAtom(id)
          atom.pos = path.pos
          List(atom)
        case TermSelect(qual: Path, id) =>
          qual.atoms ++ id.atoms
        case TermSelect(qual, id) =>
          val qualAtom = UnsupportedAtom(qual)
          qualAtom.pos = qual.pos
          List(qualAtom) ++ id.atoms
        case TermSuper(qual, mix) =>
          val thisAtom = ThisAtom(qual)
          thisAtom.pos = qual.pos
          val superAtom = SuperAtom(mix)
          superAtom.pos = mix.pos
          List(thisAtom, superAtom)
        case TermThis(qual) =>
          val atom = ThisAtom(qual)
          atom.pos = path.pos
          List(atom)
        case TptSelect(qual, id) =>
          qual.atoms ++ id.atoms
      }
    }
  }

  implicit class TptAtomsOps(tpt: Tpt) {
    def atoms: List[Atom] = {
      tpt match {
        case TptApply(tpt, args) =>
          val atom = ApplyAtom(args)
          atom.pos = tpt.pos
          tpt.atoms ++ List(atom)
        case tpt: TptPath =>
          PathAtomsOps(tpt).atoms
      }
    }
  }

  implicit class TptPathAtomsOps(tptPath: TptPath) {
    def atoms: List[Atom] = {
      PathAtomsOps(tptPath).atoms
    }
  }
}

// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.scalasig

import scala.meta.internal.{semanticdb => s}
import scala.meta.scalasig.lowlevel._

sealed trait Key
case class NameKey(name: Name) extends Key
case class RefKey(ssym: String) extends Key
case class ModuleRefKey(ssym: String) extends Key
case class SigKey(ssig: Sig) extends Key
case class TypeKey(stpe: s.Type, disambig: Int) extends Key
case object NoPreKey extends Key
case class LiteralKey(value: Any) extends Key
case class AnnotInfoKey(sann: s.Annotation) extends Key
case class SymAnnotKey(ssym: String, sann: s.Annotation) extends Key
case class MacroImplKey(simpl: String) extends Key
case class ChildrenKey(ssym: String) extends Key

object TypeKey {
  def apply(stpe: s.Type): TypeKey = {
    TypeKey(stpe, disambig(stpe))
  }

  private var nextDisambig = 1
  private def disambig(stpe: s.Type): Int = {
    var result = 0
    def unique(): Unit = {
      val disambig = nextDisambig
      nextDisambig += 1
      result = disambig
    }
    def loop(stpe: s.Type): Unit = {
      stpe match {
        case s.TypeRef(spre, _, stargs) => (spre +: stargs).foreach(loop)
        case s.SingleType(spre, _) => loop(spre)
        case s.ThisType(_) => ()
        case s.SuperType(spre, _) => loop(spre)
        case s.ConstantType(_) => ()
        case s.IntersectionType(stpes) => stpes.foreach(loop)
        case s.UnionType(stpes) => stpes.foreach(loop)
        case s.WithType(stpes) => stpes.foreach(loop)
        case s.StructuralType(_, _) => unique()
        case s.AnnotatedType(_, stpe) => loop(stpe)
        case s.ExistentialType(_, _) => unique()
        case s.UniversalType(_, _) => unique()
        case s.ByNameType(stpe) => loop(stpe)
        case s.RepeatedType(stpe) => loop(stpe)
        case s.NoType => ()
      }
    }
    loop(stpe)
    result
  }
}

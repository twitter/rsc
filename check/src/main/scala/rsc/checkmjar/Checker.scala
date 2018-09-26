// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.checkmjar

import java.nio.file._
import rsc.checkbase._
import rsc.util._
import scala.collection.mutable
import scala.meta.internal.data._
import scala.meta.internal.scalasig._
import scala.meta.scalasig._
import scala.meta.scalasig.highlevel._

class Checker(nscResult: Path, rscResult: Path) extends CheckerBase {
  def check(): Unit = {
    val nscMaps = load(nscResult, FailedNscProblem.apply)
    val rscMaps = load(rscResult, FailedRscProblem.apply)
    val names = (nscMaps.keys ++ rscMaps.keys).toList.sorted
    names.foreach { name =>
      val nscIndex = nscMaps.get(name)
      val rscIndex = rscMaps.get(name)
      (nscIndex, rscIndex) match {
        case (Some(nscIndex), Some(rscIndex)) =>
          val Index(nscSig, nscMap1) = highlevelPatch(nscIndex)
          val Index(rscSig, rscMap1) = highlevelPatch(rscIndex)
          val ids1 = (nscMap1.keys ++ rscMap1.keys).toList.sorted
          ids1.foreach { id =>
            val nscSym = nscMap1.get(id)
            val rscSym = rscMap1.get(id)
            (nscSym, rscSym) match {
              case (Some(nscSym), Some(rscSym)) =>
                highlevelPatch(nscSym)
                highlevelPatch(rscSym)
                val nscRepr = lowlevelPatch(lowlevelRepr(nscSym))
                val rscRepr = lowlevelPatch(lowlevelRepr(rscSym))
                val nscString = nscRepr.toString
                val rscString = rscRepr.toString
                if (nscString != rscString) {
                  val header = s"${rscSig.source}: $id"
                  problems += DifferentProblem(header, nscString, rscString)
                }
              case (Some(nscSym), None) =>
                problems += MissingRscProblem(s"${nscSig.source}: $id")
              case (None, Some(rscSym)) =>
                if (rscSym.name.value == "equals" ||
                    rscSym.name.value == "hashCode" ||
                    rscSym.name.value == "toString" ||
                    rscSym.id.contains("#equals().(x$1)")) {
                  // FIXME: https://github.com/twitter/rsc/issues/98
                  ()
                } else if (rscSym.id == "com.#twitter.#util.#TimeLike#<refinement>#") {
                  // FIXME: https://github.com/twitter/rsc/issues/120
                  ()
                } else {
                  problems += MissingNscProblem(s"${rscSig.source}: $id")
                }
              case (None, None) =>
                ()
            }
          }
        case (Some(nscIndex), None) =>
          problems += MissingRscProblem(s"${nscIndex.sig.source}: $name")
        case (None, Some(rscIndex)) =>
          problems += MissingNscProblem(s"${rscIndex.sig.source}: $name")
        case (None, None) =>
          ()
      }
    }
  }

  case class Index(sig: Scalasig, map: Map[String, EmbeddedSymbol])

  private def load(path: Path, problem: String => Problem): Map[String, Index] = {
    val scalasigs = mutable.Map[String, Index]()
    Scalasigs(path) {
      case ParsedScalasig(_, _, sig @ Scalasig(name, _, syms)) =>
        val map = syms.map(sym => sym.id.toString -> sym).toMap
        scalasigs(name) = Index(sig, map)
      case EmptyScalasig(_, _) =>
        ()
      case FailedScalasig(_, _, cause) =>
        problems += problem(cause.str)
      case FailedClassfile(_, cause) =>
        problems += problem(cause.str)
    }
    scalasigs.toMap
  }

  private def highlevelPatch(index: Index): Index = {
    var syms1 = index.map.values.toList
    // WONTFIX: https://github.com/twitter/rsc/issues/121
    syms1 = syms1.filter(_.isEligible)
    syms1 = syms1.filter(sym => (sym.flags & EXISTENTIAL) == 0)
    // FIXME: https://github.com/twitter/rsc/issues/101
    syms1 = syms1.filter(_.name != TypeName("<local child>"))
    Index(index.sig, syms1.map(sym => sym.id -> sym).toMap)
  }

  private def highlevelPatch(sym: EmbeddedSymbol): Unit = {
    // WONTFIX: https://github.com/twitter/rsc/issues/122
    sym.flags &= ~OVERRIDE

    // WONTFIX: https://github.com/twitter/rsc/issues/123
    sym.flags &= ~DEFAULTINIT

    // FIXME: https://github.com/twitter/rsc/issues/103
    if (sym.isInstanceOf[ValSymbol] && (sym.flags & METHOD) != 0) {
      sym.info = sym.info match {
        case PolyType(tpe, Nil) => MethodType(tpe, Nil)
        case tpe => tpe
      }
    }

    sym.info = highlevelPatch(sym.info)

    sym.thisType = sym.thisType match {
      case _: TypeRef => null
      case tpe => tpe
    }

    // FIXME: https://github.com/twitter/rsc/issues/120
    if (sym.id == "com.#twitter.#util.#TimeLike#") sym.thisType = null

    // FIXME: https://github.com/twitter/rsc/issues/93
    sym.annots = Nil

    // FIXME: https://github.com/twitter/rsc/issues/101
    sym.children = Nil
  }

  private def highlevelPatch(tpe: Type): Type = {
    def normalizePrefix(pre: Type): Type = {
      pre match {
        case SingleType(_, modSym @ ExtRef(modId)) =>
          val modClassId = modId + "#"
          val modClassSym = ExtModClassRef(modClassId)
          modClassSym.owner = modSym.owner
          modClassSym.name = modSym.name
          ThisType(modClassSym)
        case SingleType(_, modSym @ ModuleSymbol(modId)) =>
          val modClassId = modId + "#"
          val modClassSym = modSym.scalasig.symbols.find(_.id == modClassId).get
          ThisType(modClassSym)
        case other =>
          other
      }
    }
    def loop(tpe: Type): Type = {
      tpe match {
        case NoType =>
          NoType
        case NoPrefix =>
          NoPrefix
        case ThisType(sym) =>
          // FIXME: https://github.com/twitter/rsc/issues/228
          val sym1 = NoSymbol
          ThisType(sym1)
        case SingleType(pre: Type, sym: Symbol) =>
          // FIXME: https://github.com/twitter/rsc/issues/228
          val pre1 = loop(normalizePrefix(pre))
          val sym1 = sym
          SingleType(pre1, sym1)
        case ConstantType(lit) =>
          val lit1 = lit
          ConstantType(lit1)
        case TypeRef(pre, sym, targs) =>
          // FIXME: https://github.com/twitter/rsc/issues/228
          val pre1 = loop(normalizePrefix(pre))
          val sym1 = sym
          val targs1 = targs.map(loop)
          TypeRef(pre1, sym1, targs1)
        case TypeBounds(lo, hi) =>
          val lo1 = loop(lo)
          val hi1 = loop(hi)
          TypeBounds(lo1, hi1)
        case RefinedType(sym, parents) =>
          val sym1 = sym
          val parents1 = parents.map(loop)
          RefinedType(sym1, parents1)
        case ClassInfoType(sym, parents) =>
          val sym1 = sym
          val parents1 = parents.flatMap {
            // FIXME: https://github.com/twitter/rsc/issues/98
            case TypeRef(_, sym, _) if sym.id == "scala.#Serializable#" => None
            case other => Some(loop(other))
          }
          ClassInfoType(sym1, parents1)
        case MethodType(ret, params) =>
          val ret1 = loop(ret)
          val params1 = params
          MethodType(ret1, params1)
        case PolyType(tpe, params) =>
          val tpe1 = loop(tpe)
          val params1 = params
          PolyType(tpe1, params1)
        case SuperType(thisp, superp) =>
          val thisp1 = loop(thisp)
          val superp1 = loop(superp)
          SuperType(thisp1, superp1)
        case AnnotatedType(tpe, annots) =>
          val tpe1 = loop(tpe)
          val annots1 = annots.map {
            case AnnotInfo(tpe, args) =>
              // FIXME: https://github.com/twitter/rsc/issues/98
              val tpe1 = NoType
              val args1 = Nil
              AnnotInfo(tpe1, args1)
          }
          AnnotatedType(tpe1, annots1)
        case ExistentialType(tpe, decls) =>
          val tpe1 = loop(tpe)
          val decls1 = decls
          ExistentialType(tpe1, decls1)
      }
    }
    loop(tpe)
  }

  private def lowlevelRepr(sym: Symbol): Datum = {
    sym.datum
  }

  private def lowlevelPatch(datum: Datum): Datum = {
    val prettifier = new HighlevelEntityPrettifier {
      override protected def apply(field: Field): Field = {
        field match {
          case Field(_: ExistentialType, _, Message(sym: Symbol, _)) =>
            val value1 = apply(field.value)
            Field(field.owner, field.name, value1)
          case _ =>
            val field1 = super.apply(field)
            field1 match {
              case Field(_, _, Scalar(id: Id)) =>
                var id1 = id
                id1 = id1.replaceAll("_\\$(\\d+)", "_")
                // FIXME: https://github.com/twitter/rsc/issues/124
                id1 = id1.replace("#<init>().(cause)_#", "#cause._#")
                Field(field.owner, field.name, Scalar(id1))
              case _ =>
                field1
            }
        }
      }
    }
    prettifier(datum)
  }

  private implicit class SymbolOps(sym: EmbeddedSymbol) {
    def isEligible: Boolean = {
      sym.isVisible
    }

    def isVisible: Boolean = {
      val isOwnerVisible = sym.owner match {
        case owner: EmbeddedSymbol => owner.isVisible
        case _ => true
      }
      if (isOwnerVisible) {
        if ((sym.flags & PRIVATE) != 0) sym.owner.isInstanceOf[ExternalSymbol]
        else false
      } else {
        false
      }
    }
  }
}

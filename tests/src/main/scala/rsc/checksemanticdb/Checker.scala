// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.checksemanticdb

import java.nio.file._
import rsc.checkbase._
import rsc.util._
import scala.collection.mutable
import scala.meta.internal.semanticdb._
import scala.meta.internal.semanticdb.Accessibility.{Tag => a}
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.SymbolInformation.{Kind => k}
import scala.meta.internal.semanticdb.SymbolInformation.Property
import scala.meta.internal.semanticdb.SymbolInformation.{Property => p}

class Checker(nscResult: Path, rscResult: Path) extends CheckerBase {
  def check(): Unit = {
    val nscMap = load(nscResult)
    val rscMap = load(rscResult)
    val nscMap1 = highlevelPatch(nscMap)
    val rscMap1 = highlevelPatch(rscMap)
    val syms = (nscMap1.keys ++ rscMap1.keys).toList.sorted
    syms.foreach { sym =>
      val nscInfo = nscMap1.get(sym)
      val rscInfo = rscMap1.get(sym)
      (nscInfo, rscInfo) match {
        case (Some(nscInfo), Some(rscInfo)) =>
          val (nscInfo1, rscInfo1) = highlevelPatch(nscInfo, rscInfo)
          val nscRepr = lowlevelPatch(lowlevelRepr(nscInfo1))
          val rscRepr = lowlevelPatch(lowlevelRepr(rscInfo1))
          val nscString = nscRepr.toString
          val rscString = rscRepr.toString
          if (nscString != rscString) {
            problems += DifferentProblem(sym, nscString, rscString)
          }
        case (Some(nscInfo), None) =>
          if (nscInfo.symbol.contains("#_#")) {
            // FIXME: https://github.com/scalameta/scalameta/issues/1586
            ()
          } else {
            problems += MissingRscProblem(sym)
          }
        case (None, Some(rscInfo)) =>
          if (rscInfo.name == "equals" ||
              rscInfo.name == "hashCode" ||
              rscInfo.name == "toString" ||
              rscInfo.symbol.contains("#equals().(x$1)")) {
            // FIXME: https://github.com/twitter/rsc/issues/98
            ()
          } else {
            problems += MissingNscProblem(sym)
          }
        case (None, None) =>
          ()
      }
    }
  }

  private def load(path: Path): Map[String, SymbolInformation] = {
    val map = mutable.Map[String, SymbolInformation]()
    Locator(path) { (_, payload) =>
      payload.documents.foreach { document =>
        document.symbols.foreach { info =>
          if (info.symbol.isGlobal) {
            map(info.symbol) = info
          }
        }
      }
    }
    map.toMap
  }

  private def highlevelPatch(
      map: Map[String, SymbolInformation]): Map[String, SymbolInformation] = {
    var infos1 = map.values.toList
    // WONTFIX: https://github.com/scalameta/scalameta/issues/1340
    infos1 = infos1.filter(_.kind != k.PACKAGE)
    // WONTFIX: https://github.com/twitter/rsc/issues/121
    infos1 = infos1.filter { info =>
      info.kind match {
        case k.LOCAL | k.PARAMETER | k.SELF_PARAMETER | k.TYPE_PARAMETER |
            k.PACKAGE | k.PACKAGE_OBJECT =>
          // These definitions can't have any visibility modifiers.
          true
        case k.OBJECT | k.CLASS | k.TRAIT | k.INTERFACE =>
          // These definitions can be private, but for them `private`
          // sometimes means something very special.
          // When used at the top level, `private` means private to
          // the enclosing package, so we can't really skip these definitions
          // without having a symbol table.
          true
        case k.FIELD | k.METHOD | k.CONSTRUCTOR | k.MACRO | k.TYPE =>
          val acc = info.accessibility.map(_.tag).getOrElse(0)
          acc != a.PRIVATE && acc != a.PRIVATE_THIS
        case _ =>
          sys.error(info.toProtoString)
      }
    }
    infos1.map(info => info.symbol -> info).toMap
  }

  private def highlevelPatch(
      n: SymbolInformation,
      r: SymbolInformation): (SymbolInformation, SymbolInformation) = {
    val stdlib = new rsc.semantics.Stdlib {}
    import stdlib._

    var n1 = n
    var r1 = r

    def nhas(prop: Property) = (n.properties & prop.value) != 0
    def rhas(prop: Property) = (r.properties & prop.value) != 0
    if (n.kind == k.PARAMETER && nhas(p.VAL) && !rhas(p.VAL)) {
      // WONTFIX: https://github.com/scalameta/scalameta/issues/1538
      n1 = n1.copy(properties = n1.properties & ~p.VAL.value)
    }
    // FIXME: https://github.com/scalameta/scalameta/issues/1492
    r1 = r1.copy(properties = r1.properties & ~p.SYNTHETIC.value)
    // FIXME: https://github.com/scalameta/scalameta/issues/1645
    r1 = r1.copy(properties = r1.properties & ~p.DEFAULT.value)

    n.signature match {
      case ClassSignature(ntparams, nps, nself, Some(ndecls)) =>
        var nps1 = nps
        // FIXME: https://github.com/twitter/rsc/issues/98
        nps1 = nps1.filter {
          case TypeRef(_, SerializableClass, _) => false
          case _ => true
        }
        // FIXME: https://github.com/twitter/rsc/issues/120
        val nself1 = NoType
        var nds1 = ndecls.symlinks
        // FIXME: https://github.com/scalameta/scalameta/issues/1548
        nds1 = nds1.sorted
        // WONTFIX: https://github.com/twitter/rsc/issues/121
        nds1 = nds1.filter(_.desc.name != "readResolve")
        // FIXME: https://github.com/twitter/rsc/issues/98
        nds1 = nds1.filter(_.desc.name != "equals")
        // FIXME: https://github.com/twitter/rsc/issues/98
        nds1 = nds1.filter(_.desc.name != "hashCode")
        // FIXME: https://github.com/twitter/rsc/issues/98
        nds1 = nds1.filter(_.desc.name != "toString")
        // FIXME: https://github.com/scalameta/scalameta/issues/1586
        nds1 = nds1.filter(!_.contains("#_#"))
        val ndecls1 = Some(Scope(nds1))
        val nsig1 = ClassSignature(ntparams, nps1, nself1, ndecls1)
        n1 = n1.update(_.signature := nsig1)
      case MethodSignature(ntparams, npss, nret) =>
        // FIXME: https://github.com/twitter/rsc/issues/103
        if (npss.isEmpty) {
          val npss1 = List(Scope())
          n1 = n1.update(_.signature := MethodSignature(ntparams, npss1, nret))
        }
      case _ =>
        ()
    }

    r.signature match {
      case ClassSignature(rtparams, rps, rself, Some(rdecls)) =>
        var rps1 = rps
        // FIXME: https://github.com/twitter/rsc/issues/98
        rps1 = rps1.filter {
          case TypeRef(_, SerializableClass, _) => false
          case _ => true
        }
        // FIXME: https://github.com/twitter/rsc/issues/120
        val rself1 = NoType
        var rds1 = rdecls.symlinks
        // FIXME: https://github.com/scalameta/scalameta/issues/1548
        rds1 = rds1.sorted
        // WONTFIX: https://github.com/twitter/rsc/issues/121
        rds1 = rds1.filter(_.desc.name != "readResolve")
        // FIXME: https://github.com/twitter/rsc/issues/98
        rds1 = rds1.filter(_.desc.name != "equals")
        // FIXME: https://github.com/twitter/rsc/issues/98
        rds1 = rds1.filter(_.desc.name != "hashCode")
        // FIXME: https://github.com/twitter/rsc/issues/98
        rds1 = rds1.filter(_.desc.name != "toString")
        val rdecls1 = Some(Scope(rds1))
        val rsig1 = ClassSignature(rtparams, rps1, rself1, rdecls1)
        r1 = r1.update(_.signature := rsig1)
      case MethodSignature(rtparams, rpss, rret) =>
        // FIXME: https://github.com/twitter/rsc/issues/103
        val rpss1 = if (rpss.isEmpty) List(Scope()) else rpss
        r1 = r1.update(_.signature := MethodSignature(rtparams, rpss1, rret))
      case _ =>
        ()
    }

    // FIXME: https://github.com/scalameta/scalameta/issues/1315
    n1 = n1.copy(annotations = Nil)
    // FIXME: https://github.com/twitter/rsc/issues/93
    r1 = r1.copy(annotations = Nil)

    (n1, r1)
  }

  private def lowlevelRepr(info: SymbolInformation): String = {
    info.toProtoString
  }

  private def lowlevelPatch(s: String): String = {
    var s1 = s
    s1 = s1.replaceAll("symbol: \"local(\\d+)\"", "symbol: \"localNNN\"")
    s1 = s1.replaceAll("symbol: \".*?#_#\"", "symbol: \"localNNN\"")
    s1
  }
}

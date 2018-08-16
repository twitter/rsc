// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.checkoutline

import java.nio.file._
import rsc.checkbase._
import rsc.util._
import scala.collection.mutable
import scala.meta.internal.semanticdb._
import scala.meta.internal.semanticdb.Language._
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.SymbolInformation.{Kind => k}
import scala.meta.internal.semanticdb.SymbolInformation.{Property => p}
import scala.meta.internal.semanticdb.SymbolOccurrence.{Role => r}

class Checker(nscResult: Path, rscResult: Path) extends CheckerBase {
  def check(): Unit = {
    val nscIndex = load(nscResult)
    val rscIndex = load(rscResult)
    val nscIndex1 = highlevelPatch(nscIndex)
    val rscIndex1 = highlevelPatch(rscIndex)
    val syms = (nscIndex1.infos.keys ++ rscIndex1.infos.keys).toList.sorted
    syms.foreach { sym =>
      val nscInfo = nscIndex1.infos.get(sym)
      val rscInfo = rscIndex1.infos.get(sym)
      (nscInfo, rscInfo) match {
        case (Some(nscInfo), Some(rscInfo)) =>
          // FIXME: https://github.com/twitter/rsc/issues/90
          if (sym == "com/twitter/util/Credentials.parser.auth()." ||
              sym == "com/twitter/util/Credentials.parser.content()." ||
              sym == "com/twitter/util/NilStopwatch.start().") {
            ()
          } else {
            val nscInfo1 = highlevelPatch(nscIndex, nscInfo)
            val rscInfo1 = highlevelPatch(rscIndex, rscInfo)
            val nscRepr = lowlevelPatch(lowlevelRepr(nscInfo1))
            val rscRepr = lowlevelPatch(lowlevelRepr(rscInfo1))
            val nscString = nscRepr.toString
            val rscString = rscRepr.toString
            if (nscString != rscString) {
              val header = s"${rscIndex1.anchors(sym)}: $sym"
              problems += DifferentProblem(header, nscString, rscString)
            }
          }
        case (Some(nscInfo), None) =>
          if (nscInfo.symbol.contains("#_$")) {
            // FIXME: https://github.com/scalameta/scalameta/issues/1586
            ()
          } else {
            val header = s"${nscIndex1.anchors(sym)}: $sym"
            problems += MissingRscProblem(header)
          }
        case (None, Some(rscInfo)) =>
          if (rscInfo.displayName == "equals" ||
              rscInfo.displayName == "hashCode" ||
              rscInfo.displayName == "toString" ||
              rscInfo.symbol.contains("#equals().(x$1)")) {
            // FIXME: https://github.com/twitter/rsc/issues/98
            ()
          } else {
            val header = s"${rscIndex1.anchors(sym)}: $sym"
            problems += MissingNscProblem(header)
          }
        case (None, None) =>
          ()
      }
    }
  }

  case class Index(
      infos: Map[String, SymbolInformation],
      anchors: Map[String, String])

  private def load(path: Path): Index = {
    val infos = mutable.Map[String, SymbolInformation]()
    val anchors = mutable.Map[String, String]()
    Locator(path) { (_, payload) =>
      payload.documents.foreach { document =>
        val ranges = mutable.Map[String, Range]()
        document.occurrences.foreach {
          case SymbolOccurrence(Some(range), symbol, r.DEFINITION) =>
            ranges(symbol) = range
          case _ =>
            ()
        }
        document.symbols.foreach { info =>
          if (info.symbol.isGlobal) {
            infos(info.symbol) = info
            var anchor = document.uri
            ranges.get(info.symbol).foreach { range =>
              anchor += s":${range.startLine + 1}"
            }
            anchors(info.symbol) = anchor
          }
        }
      }
    }
    Index(infos.toMap, anchors.toMap)
  }

  private def highlevelPatch(index: Index): Index = {
    val indexOps = new IndexOps(index)
    import indexOps._
    var infos1 = index.infos.values.toList
    // WONTFIX: https://github.com/twitter/rsc/issues/121
    infos1 = infos1.filter(_.isEligible)
    // NOTE: https://github.com/twitter/rsc/issues/191
    infos1 = infos1.filter(_.language == SCALA)
    Index(infos1.map(info => info.symbol -> info).toMap, index.anchors)
  }

  private def highlevelPatch(
      index: Index,
      info: SymbolInformation): SymbolInformation = {
    val indexOps = new IndexOps(index)
    import indexOps._
    val stdlib = new rsc.semantics.Stdlib {}
    import stdlib._

    var info1 = info
    if (info1.kind == k.PARAMETER) {
      // WONTFIX: https://github.com/scalameta/scalameta/issues/1538
      info1 = info1.copy(properties = info1.properties & ~p.VAL.value)
      info1 = info1.copy(properties = info1.properties & ~p.VAR.value)
    }
    // FIXME: https://github.com/scalameta/scalameta/issues/1492
    info1 = info1.copy(properties = info1.properties & ~p.SYNTHETIC.value)

    info1.signature match {
      case ClassSignature(tps, ps, self, Some(ds)) =>
        var ps1 = ps
        // FIXME: https://github.com/twitter/rsc/issues/98
        ps1 = ps1.filter {
          case TypeRef(_, SerializableClass, _) => false
          case _ => true
        }
        var self1 = self
        // FIXME: https://github.com/twitter/rsc/issues/120
        if (info1.symbol == "com/twitter/util/TimeLike#") {
          self1 = NoType
        }
        var ds1 = ds.symlinks
        // FIXME: https://github.com/scalameta/scalameta/issues/1548
        ds1 = ds1.sorted
        // WONTFIX: https://github.com/twitter/rsc/issues/121
        ds1 = ds1.filter(_.info.isEligible)
        // FIXME: https://github.com/twitter/rsc/issues/98
        ds1 = ds1.filter(_.desc.value != "equals")
        // FIXME: https://github.com/twitter/rsc/issues/98
        ds1 = ds1.filter(_.desc.value != "hashCode")
        // FIXME: https://github.com/twitter/rsc/issues/98
        ds1 = ds1.filter(_.desc.value != "toString")
        // FIXME: https://github.com/scalameta/scalameta/issues/1586
        ds1 = ds1.filter(!_.contains("#_$"))
        val ndecls1 = Some(Scope(ds1))
        val nsig1 = ClassSignature(tps, ps1, self1, ndecls1)
        info1 = info1.update(_.signature := nsig1)
      case MethodSignature(tps, pss, ret) =>
        // FIXME: https://github.com/twitter/rsc/issues/103
        if (pss.isEmpty) {
          val pss1 = List(Scope())
          info1 = info1.update(_.signature := MethodSignature(tps, pss1, ret))
        }
      case _ =>
        ()
    }

    // FIXME: https://github.com/twitter/rsc/issues/93
    // FIXME: https://github.com/scalameta/scalameta/issues/1315
    info1 = info1.copy(annotations = Nil)

    info1
  }

  private def lowlevelRepr(info: SymbolInformation): String = {
    info.toProtoString
  }

  private def lowlevelPatch(s: String): String = {
    var s1 = s
    s1 = s1.replaceAll("symbol: \"local(\\d+)\"", "symbol: \"localNNN\"")
    s1 = s1.replaceAll("symbol: \".*?#_\\$(\\d+)#\"", "symbol: \"localNNN\"")
    s1
  }

  private class IndexOps(index: Index) {
    implicit class SymbolOps(sym: String) {
      def info: SymbolInformation = {
        if (sym.contains("#_$")) {
          // FIXME: https://github.com/scalameta/scalameta/issues/1586
          SymbolInformation(symbol = sym)
        } else if (sym.desc.isPackage) {
          SymbolInformation(symbol = sym, kind = k.PACKAGE)
        } else {
          index.infos(sym)
        }
      }
    }

    implicit class InfoOps(info: SymbolInformation) {
      def isEligible: Boolean = {
        info.isVisible
      }

      def isVisible: Boolean = {
        if (info.symbol.isGlobal) {
          if (info.kind == k.PACKAGE) {
            true
          } else {
            val owner = info.symbol.owner.info
            val isOwnerVisible = owner.isVisible
            if (isOwnerVisible) {
              info.access match {
                case PrivateAccess() => owner.kind == k.PACKAGE
                case PrivateThisAccess() => false
                case _ => true
              }
            } else {
              false
            }
          }
        } else {
          false
        }
      }
    }
  }
}

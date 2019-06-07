// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.checkoutline

import java.nio.file._
import rsc.checkbase._
import rsc.pretty._
import rsc.util._
import scala.collection.mutable
import scala.meta.internal.semanticdb._
// import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.Scala.{Descriptor => d}
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
          if (nscInfo.symbol == "com/twitter/util/Stopwatches.start()." ||
              nscInfo.symbol == "com/twitter/util/StopwatchBenchmark.StopwatchState#elapsed.") {
            // FIXME: https://github.com/scalameta/scalameta/issues/1782
            ()
          } else if ((nscInfo.displayName.contains("$default$") ||
                     nscInfo.displayName == "unapply") &&
                     nscInfo.str.contains("existential_type")) {
            // FIXME: https://github.com/twitter/rsc/issues/274
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
          if (sym.contains("#_$")) {
            // FIXME: https://github.com/scalameta/scalameta/issues/1586
            ()
          } else if (sym.contains("#protected$")) {
            // FIXME: https://github.com/twitter/rsc/issues/100
            ()
          } else {
            val header = s"${nscIndex1.anchors(sym)}: $sym"
            problems += MissingRscProblem(header)
          }
        case (None, Some(rscInfo)) =>
          if (sym.desc.value == "equals" ||
              sym.desc.value == "hashCode" ||
              sym.desc.value == "toString" ||
              sym.desc.value == "copy" ||
              sym.desc.value == "canEqual" ||
              sym.desc.value == "apply" ||
              sym.desc.value == "unapply" ||
              sym.contains("#equals(") ||
              sym.contains("#copy") ||
              sym.contains("#canEqual") ||
              sym.contains(".apply") ||
              sym.contains(".unapply")) {
            // FIXME: https://github.com/twitter/rsc/issues/98
            ()
          } else if (sym.desc.value == "$init$") {
            // FIXME: https://github.com/scalameta/scalameta/issues/1819
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

  case class Index(infos: Map[String, SymbolInformation], anchors: Map[String, String])

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
            info.symbol.desc match {
              case d.TypeParameter(value) if value == "_" || value.startsWith("anon$") =>
                // FIXME: https://github.com/scalameta/scalameta/issues/1830
                ()
              case _ =>
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
    }
    Index(infos.toMap, anchors.toMap)
  }

  private def highlevelPatch(index: Index): Index = {
    val indexOps = new IndexOps(index)
    import indexOps._
    var infos1 = index.infos.values.toList
    // WONTFIX: https://github.com/twitter/rsc/issues/121
    infos1 = infos1.filter(_.isEligible)
    Index(infos1.map(info => info.symbol -> info).toMap, index.anchors)
  }

  private def highlevelPatch(index: Index, info: SymbolInformation): SymbolInformation = {
    val indexOps = new IndexOps(index)
    import indexOps._
    val stdlib = new rsc.semantics.Stdlib {}
    import stdlib._

    var info1 = info
    if (info1.isParameter) {
      // WONTFIX: https://github.com/scalameta/scalameta/issues/1538
      info1 = info1.copy(properties = info1.properties & ~p.VAL.value)
      info1 = info1.copy(properties = info1.properties & ~p.VAR.value)
      // FIXME: https://github.com/scalameta/scalameta/issues/1762
      // FIXME: https://github.com/twitter/rsc/issues/212
      info1 = info1.copy(properties = info1.properties & ~p.DEFAULT.value)
    }
    // FIXME: https://github.com/scalameta/scalameta/issues/1809
    info1 = info1.copy(properties = info1.properties & ~p.OVERRIDE.value)
    // FIXME: https://github.com/scalameta/scalameta/issues/1807
    info1 = info1.copy(properties = info1.properties & ~p.ABSOVERRIDE.value)
    // FIXME: https://github.com/scalameta/scalameta/issues/1492
    // FIXME: https://github.com/twitter/rsc/issues/264
    info1 = info1.copy(properties = info1.properties & ~p.SYNTHETIC.value)

    info1.signature match {
      case ClassSignature(tps, ps, self, Some(ds)) =>
        var ps1 = ps
        // FIXME: https://github.com/twitter/rsc/issues/98
        ps1 = ps1.filter {
          case TypeRef(_, SerializableClass, _) => false
          case _ => true
        }
        // FIXME: https://github.com/twitter/rsc/issues/120
        val self1 = NoType
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
        // FIXME: https://github.com/twitter/rsc/issues/98
        ds1 = ds1.filter(_.desc.value != "copy")
        // FIXME: https://github.com/twitter/rsc/issues/98
        ds1 = ds1.filter(!_.desc.value.startsWith("copy$default$"))
        // FIXME: https://github.com/twitter/rsc/issues/98
        ds1 = ds1.filter(_.desc.value != "canEqual")
        // FIXME: https://github.com/twitter/rsc/issues/98
        ds1 = ds1.filter(_.desc.value != "apply")
        // FIXME: https://github.com/twitter/rsc/issues/98
        ds1 = ds1.filter(_.desc.value != "unapply")
        // FIXME: https://github.com/scalameta/scalameta/issues/1819
        ds1 = ds1.filter(_.desc.value != "$init$")
        // FIXME: https://github.com/scalameta/scalameta/issues/1586
        ds1 = ds1.filter(!_.contains("#_$"))
        // FIXME: https://github.com/scalameta/scalameta/issues/1586
        ds1 = ds1.filter(!_.contains("._$"))
        // FIXME: https://github.com/twitter/rsc/issues/100
        ds1 = ds1.filter(!_.contains("#protected$"))
        val ndecls1 = Some(Scope(ds1))
        val nsig1 = ClassSignature(tps, ps1, self1, ndecls1)
        info1 = info1.update(_.signature := nsig1)
      case _ =>
        ()
    }

    info1 = info1.copy(signature = highlevelPatch(info1.signature))

    // FIXME: https://github.com/twitter/rsc/issues/93
    // FIXME: https://github.com/scalameta/scalameta/issues/1315
    info1 = info1.copy(annotations = Nil)

    info1
  }

  private def highlevelPatch(sig: Signature): Signature = {
    sig match {
      case ClassSignature(tparams, parents, self, decls) =>
        val tparams1 = tparams.map(highlevelPatch)
        val parents1 = parents.map(highlevelPatch)
        val self1 = highlevelPatch(self)
        val decls1 = decls.map(highlevelPatch)
        ClassSignature(tparams1, parents1, self1, decls1)
      case MethodSignature(tparams, paramss, ret) =>
        val tparams1 = tparams.map(highlevelPatch)
        val paramss1 = paramss.map(highlevelPatch)
        val ret1 = highlevelPatch(ret)
        MethodSignature(tparams1, paramss1, ret1)
      case TypeSignature(tparams, lo, hi) =>
        val tparams1 = tparams.map(highlevelPatch)
        val lo1 = highlevelPatch(lo)
        val hi1 = highlevelPatch(hi)
        TypeSignature(tparams1, lo1, hi1)
      case ValueSignature(tpe) =>
        val tpe1 = highlevelPatch(tpe)
        ValueSignature(tpe1)
      case NoSignature =>
        NoSignature
    }
  }

  private def highlevelPatch(tpe: Type): Type = {
    tpe match {
      case TypeRef(pre, sym, targs) =>
        val pre1 = highlevelPatch(pre)
        val sym1 = highlevelPatch(sym)
        val targs1 = targs.map(highlevelPatch)
        TypeRef(pre1, sym1, targs1)
      case SingleType(pre, sym) =>
        val pre1 = highlevelPatch(pre)
        val sym1 = highlevelPatch(sym)
        SingleType(pre1, sym1)
      case ThisType(sym) =>
        val sym1 = highlevelPatch(sym)
        ThisType(sym1)
      case SuperType(pre, sym) =>
        val pre1 = highlevelPatch(pre)
        val sym1 = highlevelPatch(sym)
        SuperType(pre1, sym1)
      case ConstantType(constant) =>
        val constant1 = constant
        ConstantType(constant1)
      case IntersectionType(tpes) =>
        val tpes1 = tpes.map(highlevelPatch)
        IntersectionType(tpes1)
      case UnionType(tpes) =>
        val tpes1 = tpes.map(highlevelPatch)
        UnionType(tpes1)
      case WithType(tpes) =>
        val tpes1 = tpes.map(highlevelPatch)
        WithType(tpes1)
      case StructuralType(tpe, decls) =>
        val tpe1 = highlevelPatch(tpe)
        val decls1 = decls.map(highlevelPatch)
        StructuralType(tpe1, decls1)
      case AnnotatedType(anns, tpe) =>
        // FIXME: https://github.com/twitter/rsc/issues/93
        val anns1 = Nil
        val tpe1 = highlevelPatch(tpe)
        AnnotatedType(anns1, tpe1)
      case ExistentialType(tpe, decls) =>
        val tpe1 = highlevelPatch(tpe)
        val decls1 = decls.map(highlevelPatch)
        ExistentialType(tpe1, decls1)
      case UniversalType(tparams, tpe) =>
        val tparams1 = tparams.map(highlevelPatch)
        val tpe1 = highlevelPatch(tpe)
        UniversalType(tparams1, tpe1)
      case ByNameType(tpe) =>
        val tpe1 = highlevelPatch(tpe)
        ByNameType(tpe1)
      case RepeatedType(tpe) =>
        val tpe1 = highlevelPatch(tpe)
        RepeatedType(tpe1)
      case NoType =>
        NoType
    }
  }

  private def highlevelPatch(scope: Scope): Scope = {
    val index = Index(Map(), Map())
    val symlinks1 = scope.symlinks
    val hardlinks1 = scope.hardlinks.flatMap { hardlink =>
      // FIXME: https://github.com/scalameta/scalameta/issues/1806
      if (hardlink.displayName == "<refinement>") None
      else Some(highlevelPatch(index, hardlink))
    }
    Scope(symlinks1, hardlinks1)
  }

  private def highlevelPatch(ann: Annotation): Annotation = {
    val Annotation(tpe) = ann
    val tpe1 = highlevelPatch(tpe)
    Annotation(tpe1)
  }

  private def highlevelPatch(sym: String): String = {
    sym
  }

  private def lowlevelRepr(info: SymbolInformation): String = {
    info.str
  }

  private def lowlevelPatch(s: String): String = {
    var s1 = s
    s1 = s1.replaceAll("symbol: \"local(\\d+)\"", "symbol: \"localNNN\"")
    // FIXME: https://github.com/scalameta/scalameta/issues/1586
    s1 = s1.replaceAll("symbol: \".*?#_\\$(\\d+)#\"", "symbol: \"localNNN\"")
    // FIXME: https://github.com/scalameta/scalameta/issues/1586
    s1 = s1.replaceAll("symbol: \".*?\\._\\$(\\d+)#\"", "symbol: \"localNNN\"")
    // FIXME: https://github.com/scalameta/scalameta/issues/1797
    s1 = s1.replaceAll("symbol: \"(.*)##\"", "symbol: \"$1.\"")
    // FIXME: https://github.com/scalameta/scalameta/issues/1830
    s1 = s1.replaceAll("\\[anon\\$\\d+\\]", "[_]")
    s1
  }

  private class IndexOps(index: Index) {
    implicit class SymbolOps(sym: String) {
      def info: SymbolInformation = {
        if (sym.contains("#_$") || sym.contains("._$")) {
          // FIXME: https://github.com/scalameta/scalameta/issues/1586
          SymbolInformation(symbol = sym)
        } else if (sym.desc.isPackage) {
          SymbolInformation(symbol = sym, kind = k.PACKAGE)
        } else {
          index.infos.get(sym) match {
            case Some(info) => info
            case None => SymbolInformation(symbol = sym)
          }
        }
      }
    }

    implicit class InfoOps(info: SymbolInformation) {
      def isEligible: Boolean = {
        info.isVisible
      }

      def isVisible: Boolean = {
        if (info.symbol.isGlobal) {
          if (info.isPackage) {
            true
          } else {
            val owner = info.symbol.owner.info
            val isOwnerVisible = owner.isVisible
            if (isOwnerVisible) {
              if (info.isPrivate) owner.isPackage
              else if (info.isPrivateThis) owner.isPackage
              else true
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

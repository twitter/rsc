// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
// NOTE: This file has been partially copy/pasted from scalameta/scalameta.
package rsc.classpath

import java.nio.file._
import java.util.HashMap
import rsc.classpath.javacp._
import rsc.classpath.scalacp._
import rsc.semantics._
import rsc.util._
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.{Language => l}
// import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.SymbolInformation.{Kind => k}
import scala.meta.scalasig._
import scala.meta.scalasig.lowlevel._

final class Classpath private (index: Index) extends AutoCloseable {
  private val infos = new HashMap[String, s.SymbolInformation]
  Scalalib.synthetics.foreach(info => infos.put(info.symbol, info))
  Scalalib.packages.foreach(info => infos.put(info.symbol, info))

  def contains(sym: String): Boolean = {
    if (infos.containsKey(sym)) {
      true
    } else {
      load(sym)
      infos.containsKey(sym)
    }
  }

  def apply(sym: String): s.SymbolInformation = {
    val info = infos.get(sym)
    if (info != null) {
      info
    } else {
      load(sym)
      val info = infos.get(sym)
      if (info != null) info
      else crash(sym)
    }
  }

  private def load(sym: String): Unit = {
    val info = infos.get(sym)
    if (info == null) {
      if (sym.hasLoc) {
        if (index.contains(sym.metadataLoc)) {
          index(sym.metadataLoc) match {
            case PackageEntry() =>
              val info = s.SymbolInformation(
                symbol = sym,
                language = l.SCALA,
                kind = k.PACKAGE,
                displayName = sym.desc.value
              )
              infos.put(info.symbol, info)
            case entry: FileEntry =>
              val binary = {
                val stream = entry.openStream()
                try BytesBinary(entry.str, stream.readAllBytes())
                finally stream.close()
              }
              val payload = Scalasig.fromBinary(binary) match {
                case FailedClassfile(_, cause) => crash(cause)
                case FailedScalasig(_, _, cause) => crash(cause)
                case EmptyScalasig(_, Classfile(_, _, JavaPayload(node))) => Javacp.parse(node, index)
                case EmptyScalasig(_, Classfile(name, _, _)) => crash(name)
                case ParsedScalasig(_, _, scalasig) => Scalacp.parse(scalasig, index)
              }
              payload.foreach(info => infos.put(info.symbol, info))
          }
        }
      } else {
        if (sym.owner != "") load(sym.owner)
        else ()
      }
    }
  }

  def close(): Unit = {
    index.close()
  }
}

object Classpath {
  def apply(paths: List[Path]): Classpath = {
    val index = Index(paths)
    new Classpath(index)
  }
}

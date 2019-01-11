// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.classpath

import java.io._
import java.nio.file._
import java.util.HashMap
import java.util.jar._
import rsc.semantics._
import rsc.util._
import scala.collection.JavaConverters._
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.{Language => l}
import scala.meta.internal.semanticdb.SymbolInformation.{Kind => k}
import scala.meta.internal.{semanticidx => i}

final class Classpath private (entries: HashMap[Symbol, Entry]) extends AutoCloseable {
  private val infos = new HashMap[Symbol, s.SymbolInformation]

  def contains(sym: Symbol): Boolean = {
    if (infos.containsKey(sym)) {
      true
    } else {
      load(sym)
      infos.containsKey(sym)
    }
  }

  def apply(sym: Symbol): s.SymbolInformation = {
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

  private def load(sym: Symbol): Unit = {
    val info = infos.get(sym)
    if (info == null) {
      val entry = entries.get(sym)
      entry match {
        case PackageEntry() =>
          val info = s.SymbolInformation(
            symbol = sym,
            language = l.SCALA,
            kind = k.PACKAGE,
            displayName = sym.desc.value
          )
          infos.put(info.symbol, info)
        case entry: FileEntry =>
          val stream = entry.openStream()
          try {
            val documents = s.TextDocuments.parseFrom(stream)
            documents.documents.foreach { document =>
              document.symbols.foreach { info =>
                if (info.symbol.isGlobal) {
                  infos.put(info.symbol, info)
                }
              }
            }
          } finally {
            stream.close()
          }
        case null =>
          if (sym.owner != NoSymbol) load(sym.owner)
          else ()
      }
    }
  }

  def close(): Unit = {
    entries.values.iterator.asScala.foreach {
      case CompressedEntry(jar, _) => jar.close()
      case _ => ()
    }
  }
}

object Classpath {
  def apply(classpath: List[Path]): Classpath = {
    val entries = new HashMap[Symbol, Entry]
    def visit(path: Path): Unit = {
      def fail(): Nothing = {
        val explanation = s"""
        |$path is not a supported classpath entry.
        |Rsc only supports indexed SemanticDB classpaths.
        |Indexed SemanticDB classpaths consist of directories or jars that have:
        |  1) META-INF/semanticdb subdirectory with SemanticDB payloads.
        |  2) META-INF/semanticdb.semanticidx file with an index of the payloads.
        |Regular classpaths can be converted to SemanticDB classpaths via Metacp.
        |SemanticDB classpaths can be indexed via Metai.
        """.trim.stripMargin
        crash(explanation)
      }
      if (Files.exists(path)) {
        if (Files.isDirectory(path)) {
          val indexPath = path.resolve("META-INF/semanticdb.semanticidx")
          val semanticdbRoot = path.resolve("META-INF/semanticdb")
          if (Files.exists(indexPath)) {
            val iindexes = {
              val unbufferedStream = Files.newInputStream(indexPath)
              val stream = new BufferedInputStream(unbufferedStream)
              try i.Indexes.parseFrom(stream)
              finally stream.close()
            }
            iindexes.indexes.foreach { iindex =>
              iindex.entries.foreach {
                case (isym, i.PackageEntry()) =>
                  entries.put(isym, PackageEntry())
                case (isym, i.ToplevelEntry(iuri)) =>
                  val semanticdbPath = semanticdbRoot.resolve(iuri)
                  entries.put(isym, UncompressedEntry(semanticdbPath))
                case (isym, i.Entry.Empty) =>
                  ()
              }
            }
          } else {
            fail()
          }
        } else if (path.toString.endsWith(".jar")) {
          val jar = new JarFile(path.toFile)
          val indexEntry = jar.getEntry("META-INF/semanticdb.semanticidx")
          if (indexEntry != null) {
            val iindexes = {
              val stream = jar.getInputStream(indexEntry)
              try i.Indexes.parseFrom(stream)
              finally stream.close()
            }
            iindexes.indexes.foreach { iindex =>
              iindex.entries.foreach {
                case (isym, i.PackageEntry()) =>
                  entries.put(isym, PackageEntry())
                case (isym, i.ToplevelEntry(iuri)) =>
                  val jarEntry = jar.getEntry("META-INF/semanticdb/" + iuri)
                  entries.put(isym, CompressedEntry(jar, jarEntry))
                case (isym, i.Entry.Empty) =>
                  ()
              }
            }
          } else {
            fail()
          }
          val manifest = jar.getManifest
          if (manifest != null) {
            val classpathAttr = manifest.getMainAttributes.getValue("Class-Path")
            if (classpathAttr != null) {
              classpathAttr.split(" ").foreach { relativePath =>
                val parentPath = path.toAbsolutePath.getParent
                visit(parentPath.resolve(relativePath))
              }
            }
          }
        } else {
          ()
        }
      } else {
        ()
      }
    }
    classpath.foreach(visit)
    new Classpath(entries)
  }
}

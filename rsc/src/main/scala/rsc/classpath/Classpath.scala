// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
// NOTE: This file has been partially copy/pasted from scalameta/scalameta.
package rsc.classpath

import java.nio.file._
import java.nio.file.attribute._
import java.util.HashMap
import java.util.jar._
import rsc.semantics._
import rsc.util._
import scala.collection.JavaConverters._
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.{Language => l}
import scala.meta.internal.semanticdb.Scala.{Descriptor => d}
import scala.meta.internal.semanticdb.SymbolInformation.{Kind => k}

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
      val entry = {
        sym.desc match {
          case d.Package(value) =>
            entries.get(sym)
          case desc =>
            val key = sym.owner + desc.value + ".class"
            entries.get(key)
        }
      }
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
            ???
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
  def apply(paths: List[Path]): Classpath = {
    val entries = new HashMap[String, Entry]
    def visit(root: Path): Unit = {
      if (Files.exists(root)) {
        if (Files.isDirectory(root)) {
          Files.walkFileTree(root, new SimpleFileVisitor[Path] {
            override def visitFile(
                file: Path,
                attrs: BasicFileAttributes
            ): FileVisitResult = {
              if (file.toString.endsWith(".class")) {
                val key = root.relativize(file).toString
                entries.put(key, UncompressedEntry(file))
              }
              super.visitFile(file, attrs)
            }
            override def preVisitDirectory(
                dir: Path,
                attrs: BasicFileAttributes
            ): FileVisitResult = {
              if (dir.endsWith("META-INF")) {
                FileVisitResult.SKIP_SUBTREE
              } else {
                val key = root.relativize(dir).toString + "/"
                entries.put(key, PackageEntry())
                super.preVisitDirectory(dir, attrs)
              }
            }
          })
        } else if (root.toString.endsWith(".jar")) {
          val jar = new JarFile(root.toFile)
          val jarEntries = jar.entries()
          while (jarEntries.hasMoreElements) {
            val jarEntry = jarEntries.nextElement()
            if (jarEntry.getName.endsWith(".class") && !jarEntry.getName.startsWith("META-INF")) {
              val key = jarEntry.getName
              entries.put(key, CompressedEntry(jar, jarEntry))
              val parts = jarEntry.getName.split("/").toList.dropRight(1)
              val packages = parts.inits.toList.dropRight(1).map(parts => parts.mkString("/") + "/")
              packages.foreach(entries.put(_, PackageEntry()))
            }
          }
          val manifest = jar.getManifest
          if (manifest != null) {
            val classpathAttr = manifest.getMainAttributes.getValue("Class-Path")
            if (classpathAttr != null) {
              classpathAttr.split(" ").foreach { relativePath =>
                val parentPath = root.toAbsolutePath.getParent
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
    paths.foreach(visit)
    new Classpath(entries)
  }
}

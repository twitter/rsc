// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.symtab

import java.nio.file._
import java.util.jar._
import java.util.HashMap
import scala.meta.internal.semanticdb3._
import scala.meta.internal.semanticdb3.{Language => l}
import scala.meta.internal.semanticdb3.Scala._
import scala.meta.internal.semanticdb3.SymbolInformation.{Kind => k}
import rsc.semantics._
import rsc.typecheck._
import rsc.util._

trait Loaders {
  self: Symtab =>

  private val todo = new HashMap[Symbol, SemanticdbFile]

  // NOTE: I would've used URL instead of a custom ADT,
  // but Scala Native doesn't suppport URL.openStream.
  // It also doesn't support ZipFileSystemProvider, for that matter.
  private sealed trait SemanticdbFile
  private case class Uncompressed(dir: Path, rel: String) extends SemanticdbFile
  private case class Compressed(jar: Path, rel: String) extends SemanticdbFile

  protected def scanClasspath(classpath: List[Path]): Unit = {
    val packages = List.newBuilder[Scope]
    classpath.foreach { entryPath =>
      def consumeIndex(index: Index): Unit = {
        index.packages.foreach { p =>
          var scope = _scopes._storage.get(p.symbol)
          if (scope == null) {
            scope = PackageScope(p.symbol)
            packages += scope
            _scopes._storage.put(p.symbol, scope)
            val info = SymbolInformation(
              symbol = p.symbol,
              language = l.SCALA,
              kind = k.PACKAGE,
              name = p.symbol.desc.name,
              owner = p.symbol.owner
            )
            _infos._storage.put(p.symbol, info)
          }
          p.members.foreach { m =>
            val name = Name(m.desc.toString)
            val m1 = scope.enter(name, m)
            if (m1 != NoSymbol && m1 != m) crash(m)
          }
        }
        index.toplevels.foreach { t =>
          val semanticdbFile = {
            if (Files.isDirectory(entryPath)) {
              Uncompressed(entryPath, "META-INF/semanticdb/" + t.uri)
            } else if (entryPath.toString.endsWith(".jar")) {
              Compressed(entryPath, "META-INF/semanticdb/" + t.uri)
            } else {
              crash(entryPath.toString)
            }
          }
          todo.put(t.symbol, semanticdbFile)
        }
      }
      if (Files.isDirectory(entryPath)) {
        val indexPath = entryPath.resolve("META-INF/semanticdb.semanticidx")
        if (Files.exists(indexPath)) {
          val indexStream = Files.newInputStream(indexPath)
          try consumeIndex(Index.parseFrom(indexStream))
          finally indexStream.close()
        }
      } else if (entryPath.toString.endsWith(".jar")) {
        val jar = new JarFile(entryPath.toFile)
        val indexEntry = jar.getEntry("META-INF/semanticdb.semanticidx")
        if (indexEntry != null) {
          val indexStream = jar.getInputStream(indexEntry)
          try consumeIndex(Index.parseFrom(indexStream))
          finally indexStream.close()
        }
      } else {
        ()
      }
    }
    packages.result.foreach(_.succeed())
  }

  protected def loadFromClasspath(sym: Symbol): Unit = {
    val semanticdbFile = todo.get(sym)
    if (semanticdbFile != null) {
      val semanticdb = {
        val semanticdbStream = {
          semanticdbFile match {
            case Uncompressed(dir, rel) =>
              Files.newInputStream(dir.resolve(rel))
            case Compressed(jar, rel) =>
              val jarFile = new JarFile(jar.toFile)
              val jarEntry = jarFile.getEntry(rel)
              jarFile.getInputStream(jarEntry)
          }
        }
        try TextDocuments.parseFrom(semanticdbStream)
        finally semanticdbStream.close()
      }
      val infos = semanticdb.documents.flatMap(_.symbols)
      infos.foreach { info =>
        info.kind match {
          case k.OBJECT | k.PACKAGE_OBJECT | k.CLASS | k.TRAIT | k.INTERFACE =>
            val scope = SemanticdbScope(info, this)
            _scopes._storage.put(info.symbol, scope)
          case _ =>
            ()
        }
        _infos._storage.put(info.symbol, info)
      }
      todo.remove(sym)
    }
  }
}

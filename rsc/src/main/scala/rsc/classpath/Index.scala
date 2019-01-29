// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.classpath

import java.nio.file._
import java.nio.file.attribute._
import java.util.HashMap
import java.util.jar._
import rsc.util._
import scala.collection.JavaConverters._
import scala.meta.internal.semanticdb.Scala._

class Index private (entries: HashMap[String, Entry]) extends AutoCloseable {
  def contains(key: String): Boolean = {
    key match {
      case Symbols.RootPackage => true
      case Symbols.EmptyPackage => true
      case _ => entries.containsKey(key)
    }
  }

  def apply(key: String): Entry = {
    key match {
      case Symbols.RootPackage =>
        PackageEntry()
      case Symbols.EmptyPackage =>
        PackageEntry()
      case _ =>
        val entry = entries.get(key)
        if (entry != null) entry
        else crash(key)
    }
  }

  def close(): Unit = {
    entries.values.iterator.asScala.foreach {
      case CompressedEntry(jar, _) => jar.close()
      case _ => ()
    }
  }
}

object Index {
  def apply(paths: List[Path]): Index = {
    val entries = new HashMap[String, Entry]
    def visit(root: Path): Unit = {
      if (Files.exists(root)) {
        if (Files.isDirectory(root)) {
          Files.walkFileTree(
            root,
            new SimpleFileVisitor[Path] {
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
                  if (dir != root) {
                    val key = root.relativize(dir).toString + "/"
                    entries.put(key, PackageEntry())
                  }
                  super.preVisitDirectory(dir, attrs)
                }
              }
            }
          )
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
    new Index(entries)
  }
}

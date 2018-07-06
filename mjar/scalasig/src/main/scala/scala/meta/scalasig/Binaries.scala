// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
// NOTE: This file has been partially copy/pasted from scalameta/scalameta.
package scala.meta.scalasig

import java.net._
import java.nio.file._
import java.util.jar._
import scala.collection.JavaConverters._

object Binaries {
  def apply(paths: List[Path])(fn: Binary => Unit): Unit = {
    paths.foreach(path => apply(path)(fn))
  }

  def apply(path: Path)(fn: Binary => Unit): Unit = {
    if (Files.isDirectory(path)) {
      Files
        .walk(path)
        .iterator()
        .asScala
        .filter(_.toString.endsWith(".class"))
        .toArray
        // NOTE: nio.file.Path.compareTo is file system specific,
        // and the behavior is different on windows vs. unix
        .sortBy(_.toString.toLowerCase)
        .foreach(path => fn(path))
    } else {
      if (path.toString.endsWith(".jar")) {
        // NOTE: Can't use nio.Files.walk because nio.FileSystems
        // is not supported on Scala Native.
        val jar = new JarFile(path.toFile)
        val buf = List.newBuilder[JarEntry]
        val jarIt = jar.entries()
        while (jarIt.hasMoreElements) {
          val jarEntry = jarIt.nextElement()
          if (jarEntry.getName.endsWith(".class")) {
            buf += jarEntry
          }
        }
        val jarEntries = buf.result.sortBy(_.getName.toLowerCase)
        jarEntries.foreach { jarEntry =>
          fn(new URL("jar:file:" + path + "!/" + jarEntry.getName).toURI)
        }
        val manifest = jar.getManifest
        if (manifest != null) {
          val classpathAttr = manifest.getMainAttributes.getValue("Class-Path")
          if (classpathAttr != null) {
            classpathAttr.split(" ").foreach { relativePath =>
              val parentPath = path.toAbsolutePath.getParent
              apply(parentPath.resolve(relativePath))(fn)
            }
          }
        }
      } else if (path.toString.endsWith(".class")) {
        fn(path)
      } else {
        ()
      }
    }
  }
}

// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scala.meta.internal.mjar

import java.io._
import java.nio.file._
import java.util.jar._
import scala.collection.mutable
import scala.meta.cli._
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.Scala.{Descriptor => d}
import scala.meta.mjar._
import scala.meta.scalasig._

class Main(settings: Settings, reporter: Reporter) {
  def process(): Option[Path] = {
    try {
      val in = settings.classpath
      val out = settings.out
      Files.createDirectories(out.toAbsolutePath.getParent)
      val os = Files.newOutputStream(out)
      val bos = new BufferedOutputStream(os)
      val jos = new JarOutputStream(bos)
      try {
        val symtab = Symtab(settings)
        val done = mutable.HashSet[String]()
        symtab.todo.foreach { sym =>
          if (!done(sym)) {
            try {
              val companionSym = {
                val desc = sym.desc
                if (desc.isTerm) Symbols.Global(sym.owner, d.Type(desc.value))
                else Symbols.Global(sym.owner, d.Term(desc.value))
              }

              val pickle = new Pickle(settings.abi, symtab, sym, companionSym)
              pickle.emitEmbeddedSym(sym, ToplevelMode)
              done += sym
              if (symtab.contains(companionSym)) {
                pickle.emitEmbeddedSym(companionSym, ToplevelMode)
                done += companionSym
              }

              val scalasig = pickle.toScalasig
              val classfile = scalasig.toClassfile
              jos.putNextEntry(new JarEntry(classfile.name + ".class"))
              jos.write(classfile.toBinary)
              jos.closeEntry()

              if (symtab.contains(companionSym)) {
                val markerName = classfile.name + "$"
                val markerSource = classfile.source
                val markerClassfile = Classfile(markerName, markerSource, None)
                jos.putNextEntry(new JarEntry(markerClassfile.name + ".class"))
                jos.write(markerClassfile.toBinary)
                jos.closeEntry()
              }
            } catch {
              case ex: Throwable =>
                throw ConvertException(in, sym, ex)
            }
          }
        }
        Some(out)
      } finally {
        jos.close()
        bos.close()
        os.close()
      }
    } catch {
      case ex: Throwable =>
        ex.printStackTrace(reporter.err)
        None
    }
  }
}

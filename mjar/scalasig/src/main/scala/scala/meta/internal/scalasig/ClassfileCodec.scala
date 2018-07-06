// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scala.meta.internal.scalasig

import org.objectweb.asm._
import org.objectweb.asm.ClassReader._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.tree._
import scala.collection.JavaConverters._
import scala.meta.scalasig._

object ClassfileCodec {
  def fromBinary(binary: Binary): Classfile = {
    val is = binary.openStream()
    try {
      val node = new ClassNode()
      val classReader = new ClassReader(is)
      classReader.accept(node, SKIP_DEBUG | SKIP_FRAMES | SKIP_CODE)
      val attrs = if (node.attrs != null) node.attrs.asScala else Nil
      if (attrs.exists(_.`type` == "ScalaSig")) {
        node.visibleAnnotations.asScala.foreach { ann =>
          if (ann.desc == "Lscala/reflect/ScalaSignature;") {
            ann.values.asScala.toList match {
              case List("bytes", packedScalasig: String) =>
                val bs = ClassfileReader.unpackScalasig(Array(packedScalasig))
                return Classfile(node.name, Some(bs))
              case _ =>
                ()
            }
          } else if (ann.desc == "Lscala/reflect/ScalaLongSignature;") {
            ann.values.asScala.toList match {
              case List("bytes", packedScalasig: Array[String]) =>
                val bs = ClassfileReader.unpackScalasig(packedScalasig)
                return Classfile(node.name, Some(bs))
              case _ =>
                ()
            }
          }
        }
      }
      Classfile(node.name, None)
    } finally {
      is.close()
    }
  }

  def toBinary(classfile: Classfile): Array[Byte] = {
    val classWriter = new ClassWriter(0)
    classWriter.visit(
      V1_8,
      ACC_PUBLIC + ACC_SUPER,
      classfile.name,
      null,
      "java/lang/Object",
      null)
    classfile.scalasigBytes.foreach { unpackedScalasig =>
      val packedScalasig = ClassfileWriter.packScalasig(unpackedScalasig)
      packedScalasig match {
        case Array(packedScalasig) =>
          val desc = "Lscala/reflect/ScalaSignature;"
          val av = classWriter.visitAnnotation(desc, true)
          av.visit("bytes", packedScalasig)
          av.visitEnd()
        case packedScalasigChunks =>
          val desc = "Lscala/reflect/ScalaLongSignature;"
          val av = classWriter.visitAnnotation(desc, true)
          val aav = av.visitArray("bytes")
          packedScalasigChunks.foreach(aav.visit("bytes", _))
          aav.visitEnd()
          av.visitEnd()
      }
      classWriter.visitAttribute(new PickleMarker)
      classWriter.visitEnd()
    }
    classWriter.toByteArray
  }
}

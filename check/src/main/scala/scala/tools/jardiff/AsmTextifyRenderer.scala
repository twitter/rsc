/*
 * Copyright (C) 2017 Lightbend Inc. <http://www.lightbenc.com>
 */
// NOTE: This file has been partially copy/pasted from scala/jardiff.

package scala.tools.jardiff

import java.io.PrintWriter
import java.nio.file.{Files, Path}

import scala.collection.JavaConverters._
import org.objectweb.asm.{ClassReader, Opcodes}
import org.objectweb.asm.tree.{ClassNode, FieldNode, InnerClassNode, MethodNode}
import org.objectweb.asm.util.TraceClassVisitor

class AsmTextifyRenderer(code: Boolean, raw: Boolean, privates: Boolean) extends FileRenderer {
  def outFileExtension: String = ".asm"
  override def render(in: Path, out: Path): Unit = {
    val classBytes = Files.readAllBytes(in)
    val rawNode = classFromBytes(classBytes)
    val node = if (raw) rawNode else zapScalaClassAttrs(sortClassMembers(rawNode))
    if (!code)
      node.methods.forEach(_.instructions.clear())
    if (!privates) {
      node.methods.removeIf((m: MethodNode) => isPrivate(m.access))
      node.fields.removeIf((m: FieldNode) => isPrivate(m.access))
      node.innerClasses.removeIf((m: InnerClassNode) => isPrivate(m.access))
    }
    Files.createDirectories(out.getParent)
    val pw = new PrintWriter(Files.newBufferedWriter(out))
    try {
      val trace = new TraceClassVisitor(pw)
      node.accept(trace)
    } finally {
      pw.close()
    }
  }

  private def isPrivate(access: Int): Boolean = {
    (access & Opcodes.ACC_PRIVATE) != 0

  }

  def sortClassMembers(node: ClassNode): node.type = {
    node.fields.sort(_.name compareTo _.name)
    node.methods.sort(_.name compareTo _.name)
    node
  }

  private def isScalaSigAnnot(desc: String) =
    List("Lscala/reflect/ScalaSignature", "Lscala/reflect/ScalaLongSignature").exists(desc.contains)

  // drop ScalaSig annotation and class attributes
  private def zapScalaClassAttrs(node: ClassNode): node.type = {
    if (node.visibleAnnotations != null)
      node.visibleAnnotations = node.visibleAnnotations.asScala.filterNot(a => a == null || isScalaSigAnnot(a.desc)).asJava

    node.attrs = null
    node
  }

  private def classFromBytes(bytes: Array[Byte]): ClassNode = {
    val node = new ClassNode()
    new ClassReader(bytes).accept(node, if (raw) 0 else ClassReader.SKIP_DEBUG | ClassReader.SKIP_FRAMES)

    node
  }

}

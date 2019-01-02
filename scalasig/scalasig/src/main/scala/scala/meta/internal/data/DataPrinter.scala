// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scala.meta.internal.data

final class DataPrinter {
  protected var sb = new StringBuilder
  protected var indentation = 0
  protected var afterNewline = true
  protected var messages = List.empty[Message]

  final def print(datum: Datum): Unit = {
    printDatum(datum)
  }

  final override def toString: String = {
    sb.toString
  }

  protected def append(s: String): Unit = {
    if (afterNewline) {
      sb.append("  " * indentation)
      afterNewline = false
    }
    sb.append(s)
    afterNewline = s.contains(EOL)
  }

  protected def indent(n: Int = 1): Unit = {
    indentation += n
  }

  protected def unindent(n: Int = 1): Unit = {
    indentation -= n
  }

  protected def newline(): Unit = {
    append(EOL)
  }

  protected def printDatum(datum: Datum): Unit = {
    datum match {
      case message: Message => printMessage(message)
      case scalar: Scalar => printScalar(scalar)
      case literal: Literal => printLiteral(literal)
    }
  }

  protected def printMessage(message: Message): Unit = {
    messages = message :: messages
    append("{")
    indent()
    message.fields.foreach { field =>
      newline()
      printField(field)
    }
    unindent()
    newline()
    append("}")
    messages = messages.tail
  }

  protected def printField(field: Field): Unit = {
    append(field.name)
    field.value match {
      case _: Message => append(" ")
      case _: Scalar => append(": ")
      case _: Literal => append(": ")
    }
    printDatum(field.value)
  }

  protected def printScalar(scalar: Scalar): Unit = {
    scalar.value match {
      case null => append("null")
      case value => append(value.toString)
    }
  }

  protected def printLiteral(literal: Literal): Unit = {
    literal.value match {
      case () => append("()")
      case value: Boolean => append(value.toString)
      case value: Byte => append(value.toString + "b")
      case value: Short => append(value.toString + "s")
      case value: Char => append("'" + value + "'")
      case value: Int => append(value.toString)
      case value: Long => append(value.toString + "L")
      case value: Float => append(value.toString + "f")
      case value: Double => append(value.toString)
      case value: String => append("\"" + value + "\"")
      case null => append("null")
      case value => append(value.toString)
    }
  }
}

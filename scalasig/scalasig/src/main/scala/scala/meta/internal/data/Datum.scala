// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scala.meta.internal.data

sealed trait Datum {
  final override def toString: String = {
    val printer = new DataPrinter
    printer.print(this)
    printer.toString
  }
}

case class Literal(value: Any) extends Datum

case class Scalar(value: Any) extends Datum

case class Field(owner: Any, name: String, value: Datum)

class Message(val value: Any, _fields: () => List[Field]) extends Datum {
  lazy val fields: List[Field] = _fields()
}

object Message {
  def apply(value: Any, fields: () => List[Field]): Message = {
    new Message(value, fields)
  }

  def unapply(message: Message): Option[(Any, List[Field])] = {
    Some((message.value, message.fields))
  }
}

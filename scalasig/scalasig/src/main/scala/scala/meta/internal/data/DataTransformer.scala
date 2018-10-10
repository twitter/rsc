// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scala.meta.internal.data

class DataTransformer {
  protected var stack: List[Any] = List.empty[Any]
  protected def owner: Any = stack.head

  def apply(datum: Datum): Datum = {
    datum match {
      case Message(value, fields) =>
        stack = value :: stack
        try {
          val fields1 = apply(fields)
          val fields2 = fields1.map(apply)
          Message(value, () => fields2)
        } finally {
          stack = stack.tail
        }
      case scalar: Scalar =>
        scalar
      case literal: Literal =>
        literal
    }
  }

  protected def apply(fields: List[Field]): List[Field] = {
    fields
  }

  protected def apply(field: Field): Field = {
    val value1 = apply(field.value)
    Field(field.owner, field.name, value1)
  }
}

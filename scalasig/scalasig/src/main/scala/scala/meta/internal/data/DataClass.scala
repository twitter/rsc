// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scala.meta.internal.data

import java.lang.reflect.Modifier._

trait DataClass extends Product {
  final def title: String = {
    productPrefix.take(1).toLowerCase + productPrefix.drop(1)
  }

  final def datum: Datum = {
    def fields: List[Field] = {
      val buf = List.newBuilder[Field]
      var javaFields = this.getClass.getDeclaredFields.toList
      javaFields = javaFields.filter(f => (f.getModifiers & STATIC) == 0)
      javaFields.zipWithIndex.foreach {
        case (f, i) =>
          def append(name: String, datum: Datum): Unit = {
            buf += Field(this, name, datum)
          }
          def loop(name: String, value: Any): Unit = {
            value match {
              case values: Iterable[_] => values.foreach(loop(name, _))
              case values: Array[_] => values.foreach(loop(name, _))
              case value: Option[_] => value.foreach(loop(name, _))
              case value: DataClass => append(name, value.datum)
              case value @ () => ()
              case value: Boolean => append(name, Literal(value))
              case value: Byte => append(name, Literal(value))
              case value: Short => append(name, Literal(value))
              case value: Char => append(name, Literal(value))
              case value: Int => append(name, Literal(value))
              case value: Long => append(name, Literal(value))
              case value: Float => append(name, Literal(value))
              case value: Double => append(name, Literal(value))
              case value: String => append(name, Literal(value))
              case value @ null => ()
              case value => append(name, Scalar(value))
            }
          }
          val name = f.getName
          val value = {
            f.setAccessible(true)
            f.get(this)
          }
          loop(name, value)
      }
      buf.result
    }
    Message(this, () => fields)
  }

  override def toString: String = {
    datum match {
      case message: Message => title + " " + datum.toString
      case _ => datum.toString
    }
  }
}

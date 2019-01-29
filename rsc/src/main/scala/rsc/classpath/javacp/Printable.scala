// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
// NOTE: This file has been partially copy/pasted from scalameta/scalameta.
package rsc.classpath.javacp

/** Helper to print parsed JavaTypeSignature back into original signature */
trait Printable {
  def print(sb: StringBuilder): Unit
  final def pretty: String = {
    val sb = new StringBuilder
    this.print(sb)
    sb.toString
  }
}

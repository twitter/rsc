/*
rules = "scala:rsc.rules.RscCompat"
 */
package com.twitter.util

trait Extractable[T] {
  def apply(): T
}

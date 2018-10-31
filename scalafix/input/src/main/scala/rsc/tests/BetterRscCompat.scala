/*
rules = "scala:rsc.rules.RscCompat"
RscCompat.better = true
 */
package rsc.tests

import scala.collection.mutable

object BetterRscCompat_Test {
  def x1 = 42
  def x2 = mutable.Map(1 -> "1")
}

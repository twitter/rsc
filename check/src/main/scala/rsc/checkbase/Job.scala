// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.checkbase

case class Job[T](xs: List[T], settings: SettingsBase) {
  def foreach(fn: T => Unit): Unit = {
    val startStamp = timestamp()
    var lastStamp = 0.0
    var n = xs.length
    var i = 0
    val it = xs.iterator
    while (it.hasNext) {
      val item = it.next()
      fn(item)
      i += 1
      if ((i % 100) == 0) {
        val currentStamp = timestamp()
        if (currentStamp - lastStamp > 3.0) {
          lastStamp = currentStamp
          val elapsedSeconds = currentStamp - startStamp
          val remainingSeconds = elapsedSeconds * (n - i) / i
          val remaining = "%.2f".format(remainingSeconds) + "s"
          println(s"($i / $n) $remaining remaining")
        }
      }
    }
    val elapsedSeconds = timestamp() - startStamp
    val elapsed = "%.2f".format(elapsedSeconds) + "s"
    println(s"Job finished in $elapsed")
  }

  private def timestamp(): Double = {
    1.0 * System.nanoTime() / 1000000000
  }
}

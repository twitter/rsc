package rsc.util

import rsc.report.{Reporter, VerboseMessage}
import rsc.settings.Settings

trait TimeUtil {

  def profile[A](settings: Settings, reporter: Reporter, msg: String)(fn: => A): A = {
    val start = System.nanoTime()

    val res = fn

    val end = System.nanoTime()
    val ms = (end - start) / 1000000

    if (settings.xprint("timings")) {
      reporter.append(VerboseMessage(s"Finished $msg in $ms ms"))
    }

    res
  }

  def time[A](fn: => A): (A, Long) = {
    val start = System.nanoTime()

    val res = fn

    val end = System.nanoTime()
    val ms = (end - start) / 1000000

    (res, ms)
  }

}

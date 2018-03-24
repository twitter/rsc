// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.bench

import rsc.tests._

trait CliBench extends FileFixtures {
  def run(name: String, command: List[String], runs: Int, iters: Int): Unit = {
    println(s"Running ${command.mkString(" ")} $runs x $iters times...")
    val times = 1.to(runs).map { i =>
      val start = System.nanoTime()
      val process = new java.lang.ProcessBuilder()
      process.command((command ++ List("--iters", iters.toString)): _*)
      process.directory(buildRoot.toFile)
      process.redirectOutput(ProcessBuilder.Redirect.INHERIT)
      process.redirectError(ProcessBuilder.Redirect.INHERIT)
      val exitcode = process.start().waitFor()
      if (exitcode != 0) {
        sys.error(s"Command has failed with code $exitcode")
      }
      val end = System.nanoTime()
      val result = 1.0 * (end - start) / 1000000
      println(s"Run $i: $result ms")
      result
    }
    val score = times.sum / (runs * iters)
    val s_score = "%.3f".format(score)
    val s_error = "???"

    println()
    println("Result \"" + "rsc.bench." + name + ".run\":")
    println(s"  N = $runs")
    println(s"  mean = $s_score ms/op")
    println("")

    val header = List("Benchmark", "Mode", "Cnt", "Score", "Units")
    val results = List(name + ".run", "cli", runs.toString, s_score, "ms/op")
    val table = List[List[String]](header, results)
    val widths = 0.to(header.length - 1).map(i => table.map(_(i).length).max)
    table.map(_.zip(widths).zipWithIndex).zipWithIndex.foreach {
      case (row, i) =>
        row.foreach {
          case ((cell, width), j) =>
            if (i == 0 && j == 0) {
              print(cell.padTo(width, ' '))
            } else {
              print(cell.reverse.padTo(width, ' ').reverse)
            }
            print("  ")
        }
        println("")
    }
  }
}

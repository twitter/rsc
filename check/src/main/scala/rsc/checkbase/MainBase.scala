// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.checkbase

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file._
import rsc.pretty._
import scala.collection.mutable
import scala.util._

trait MainBase[S <: SettingsBase, I, N, R]
    extends DiffUtil
    with NscUtil
    with ToolUtil {
  def main(args: Array[String]): Unit = {
    val expandedArgs = {
      args match {
        case Array(arg) if arg.startsWith("@") =>
          val argPath = Paths.get(arg.substring(1))
          val argText = new String(Files.readAllBytes(argPath), UTF_8)
          argText.split(EOL).map(_.trim).filter(_.nonEmpty).toList
        case other =>
          other.toList
      }
    }
    settings(expandedArgs) match {
      case Right(settings) =>
        val problems = process(settings)
        System.out.flush()
        if (problems.nonEmpty) sys.exit(1) else sys.exit(0)
      case Left(failures) =>
        failures.foreach(println)
        sys.exit(1)
    }
  }

  def process(settings: S): List[Problem] = {
    val allProblems = mutable.UnrolledBuffer[Problem]()
    def report(problem: Problem): Unit = {
      println(problem)
      allProblems += problem
    }

    val job = Job(inputs(settings), settings)
    job.foreach { input =>
      (nscResult(input), rscResult(input)) match {
        case (Left(nscFailures), _) =>
          val nscProblems = nscFailures.map(FailedNscProblem)
          nscProblems.foreach(report)
        case (_, Left(rscFailures)) =>
          val rscProblems = rscFailures.map { rscFailure =>
            if (rscFailure.contains(".CrashException") &&
                !rscFailure.contains(input.toString)) {
              FailedRscProblem(s"$input: $rscFailure")
            } else {
              FailedRscProblem(rscFailure)
            }
          }
          rscProblems.foreach(report)
        case (Right(nscResult), Right(rscResult)) =>
          val checker = this.checker(settings, nscResult, rscResult)
          checker.check()
          checker.problems.foreach(report)
      }
    }

    val numProblems = allProblems.length
    if (numProblems == 0) ()
    else if (numProblems == 1) println("one problem found")
    else if (numProblems == 2) println("two problems found")
    else if (numProblems == 3) println("three problems found")
    else if (numProblems == 4) println("four problems found")
    else println(s"$numProblems problems found")
    allProblems.toList
  }

  def settings(args: List[String]): Either[List[String], S]
  def inputs(settings: S): List[I]
  def nscResult(input: I): Either[List[String], N]
  def rscResult(input: I): Either[List[String], R]
  def checker(settings: S, nscResult: N, rscResult: R): CheckerBase
}

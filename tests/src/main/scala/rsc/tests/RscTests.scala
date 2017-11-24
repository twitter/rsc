// Copyright (c) 2017 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.tests

import utest._
import rsc.bench._
import rsc.Compiler
import rsc.report._
import rsc.syntax._

trait RscTests extends TestSuite with RscFixtures with FileFixtures {
  def assertEquals[A](actual: A, expect: A): Unit =
    assert(actual == expect)

  def assertNotEquals[A](actual: A, expect: A): Unit =
    assert(actual != expect)

  def assertRun(compiler: Compiler): Unit = {
    compiler.run()
    val problems = compiler.reporter.problems
    if (problems.nonEmpty) {
      problems.foreach(println)
      assert(false)
    }
  }

  def assertStructure(actual: Tree, expect: Tree): Unit = {
    def fail(x: Any, y: Any): Unit = {
      (x, y) match {
        case (x: Tree, y: Tree) =>
          println(VerboseMessage(x.pos, "actual"))
          println(VerboseMessage(y.pos, "expect"))
        case _ =>
          println(s"$x != $y")
      }
      assert(false)
    }
    def loop(x: Any, y: Any): Unit = {
      (x, y) match {
        case (xs: List[_], ys: List[_]) =>
          if (xs.length != ys.length) {
            fail(xs, ys)
          }
          xs.zip(ys).foreach { case (x, y) => loop(x, y) }
        case (xopt: Option[_], yopt: Option[_]) =>
          if (xopt.isEmpty ^ yopt.isEmpty) {
            fail(xopt, yopt)
          }
          xopt.zip(yopt).foreach { case (x, y) => loop(x, y) }
        case (x: Tree, y: Tree) =>
          if (x.productPrefix != y.productPrefix) {
            fail(x, y)
          }
          loop(x.productIterator.toList, y.productIterator.toList)
        case (x, y) =>
          if (x != y) {
            fail(x, y)
          }
      }
    }
    loop(actual, expect)
  }
}

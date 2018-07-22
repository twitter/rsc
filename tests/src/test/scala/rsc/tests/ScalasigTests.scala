// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.tests

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file._
import org.scalatest.exceptions._
import org.scalatest.tagobjects._
import rsc.checkbase._
import rsc.util._
import scala.collection.mutable
import scala.meta.scalasig._
import scala.meta.scalasig.lowlevel._

class ScalasigTests extends RscTests {
  test("scalasigs for core", Slow) {
    var scalasigActuals = mutable.Map[String, Scalasig]()
    var numProblems = 0
    Scalasigs(coreClasspath) { result =>
      try {
        result match {
          case ParsedScalasig(_, classfile1, scalasig1) =>
            scalasigActuals(scalasig1.name) = scalasig1
            val scalasigBytes1 = classfile1.scalasigBytes.get
            val classfile2 = scalasig1.toClassfile
            val scalasigBytes2 = classfile2.scalasigBytes.get
            assertEquals(
              "scalasigBytes1",
              scalasigBytes1,
              "scalasigBytes2",
              scalasigBytes2)
            Scalasig.fromClassfile(classfile2) match {
              case ParsedScalasig(_, _, scalasig2) =>
                val scalasigStr1 = scalasig1.toHighlevel.toString
                val scalasigStr2 = scalasig2.toHighlevel.toString
                assertEquals(
                  "scalasig1",
                  scalasigStr1,
                  "scalasig2",
                  scalasigStr2)
                val classfile3 = scalasig2.toClassfile
                val classfileBytes2 = classfile2.toBinary
                val classfileBytes3 = classfile3.toBinary
                assertEquals(
                  "classfileBytes2",
                  classfileBytes2,
                  "classfileBytes3",
                  classfileBytes3)
              case EmptyScalasig(_, _) =>
                fail(s"failed to parse classfile2: ${classfile2.dump()}")
              case FailedScalasig(_, _, cause) =>
                val header = s"failed to parse classfile2: ${classfile2.dump()}"
                fail(s"$header: ${cause.str}")
              case FailedClassfile(_, cause) =>
                val header = s"failed to parse classfile2: ${classfile2.dump()}"
                fail(s"$header: ${cause.str}")
            }
          case EmptyScalasig(_, _) =>
            ()
          case FailedScalasig(_, _, cause) =>
            throw cause
          case FailedClassfile(_, cause) =>
            throw cause
        }
      } catch {
        case ex: TestFailedException =>
          println(s"${ex.getClass}: ${result.binary}: ${ex.getMessage}")
          numProblems += 1
        case ex: Throwable =>
          println(ex.str)
          numProblems += 1
      }
    }
    scalasigExpects.foreach {
      case (scalasigName, pathsExpect) =>
        scalasigActuals.get(scalasigName) match {
          case Some(scalasigActual) =>
            pathsExpect.foreach { pathExpect =>
              val actual = {
                if (pathExpect.toString.endsWith(".lowlevel")) {
                  Some(scalasigActual.toString)
                } else if (pathExpect.toString.endsWith(".highlevel")) {
                  Some(scalasigActual.toHighlevel.toString)
                } else {
                  println(s"unsupported $pathExpect")
                  numProblems += 1
                  None
                }
              }
              actual match {
                case Some(actual) =>
                  val expect = new String(Files.readAllBytes(pathExpect), UTF_8)
                  try assertEquals(actual, expect)
                  catch {
                    case ex: TestFailedException =>
                      println(s"${ex.getClass}: $pathExpect: ${ex.getMessage}")
                      numProblems += 1
                  }
                case None =>
                  ()
              }
            }
          case None =>
            println(s"missing $scalasigName")
            numProblems += 1
        }
    }
    if (numProblems == 0) {
      ()
    } else {
      if (numProblems == 1) println("one error found")
      else if (numProblems == 2) println("two errors found")
      else if (numProblems == 3) println("three errors found")
      else if (numProblems == 4) println("four errors found")
      else println(s"$numProblems errors found")
      fail()
    }
  }
}

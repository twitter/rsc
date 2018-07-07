// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.outline

import rsc.pretty._
import rsc.util._
import scala.collection.mutable

abstract class Work extends Pretty {
  var status: Status = PendingStatus

  def block(dep: Work): Unit = {
    // FIXME: https://github.com/twitter/rsc/issues/104
    if (dep == null) return
    status match {
      case PendingStatus =>
        dep.status match {
          case PendingStatus =>
            status = BlockedStatus(dep)
          case BlockedStatus(depdep) =>
            val visited = mutable.Set[Work]()
            def loop(depdep: Work): Unit = {
              depdep.status match {
                case BlockedStatus(depdepdep) =>
                  if (visited(depdepdep)) {
                    val root = depdep
                    def loop(work: Work): List[Work] = {
                      if (work == root) {
                        work :: Nil
                      } else {
                        val BlockedStatus(workdep) = work.status
                        work :: loop(workdep)
                      }
                    }
                    val cycle = loop(depdepdep)
                    cycle.foreach(_.status = CyclicStatus(cycle))
                    val stuck = visited.toSet -- cycle
                    stuck.foreach(_.status = ErrorStatus)
                  } else {
                    visited += depdep
                    loop(depdepdep)
                  }
                case _: FailedStatus =>
                  status = ErrorStatus
                case _ =>
                  ()
              }
            }
            status = BlockedStatus(dep)
            visited += this
            visited += dep
            loop(depdep)
          case _: FailedStatus =>
            status = ErrorStatus
          case SucceededStatus =>
            crash(this)
        }
      case _ =>
        crash(this)
    }
  }

  def unblock(): Unit = {
    status match {
      case BlockedStatus(dep) =>
        dep.status match {
          case SucceededStatus =>
            status = PendingStatus
          case other =>
            status = PendingStatus
            block(dep)
        }
      case _ =>
        ()
    }
  }

  def fail(): Unit = {
    status match {
      case PendingStatus =>
        status = ErrorStatus
      case _ =>
        crash(this)
    }
  }

  def succeed(): Unit = {
    status match {
      case PendingStatus =>
        status = SucceededStatus
      case _ =>
        crash(this)
    }
  }

  def printStr(p: Printer): Unit = {
    PrettyWork.str(p, this)
  }

  def printRepl(p: Printer): Unit = {
    PrettyWork.repl(p, this)
  }
}

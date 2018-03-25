// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.typecheck

import rsc.pretty._

sealed trait Status extends Pretty with Product {
  def isIncomplete: Boolean = this.isInstanceOf[IncompleteStatus]
  def isPending: Boolean = this == PendingStatus
  def isBlocked: Boolean = this.isInstanceOf[BlockedStatus]
  def isComplete: Boolean = this.isInstanceOf[CompleteStatus]
  def isFailed: Boolean = this.isInstanceOf[FailedStatus]
  def isCyclic: Boolean = this.isInstanceOf[CyclicStatus]
  def isSucceeded: Boolean = this == SucceededStatus
  override def printStr(p: Printer): Unit = PrettyStatus.str(p, this)
  override def printRepl(p: Printer): Unit = PrettyStatus.repl(p, this)
}

sealed trait IncompleteStatus extends Status
final case object PendingStatus extends IncompleteStatus
final case class BlockedStatus(scope: Scope) extends IncompleteStatus

sealed trait CompleteStatus extends Status
sealed trait FailedStatus extends CompleteStatus
final case class CyclicStatus(scopes: List[Scope]) extends FailedStatus
final case object ErrorStatus extends FailedStatus
final case object SucceededStatus extends CompleteStatus

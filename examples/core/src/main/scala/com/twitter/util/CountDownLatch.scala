package com.twitter.util

import java.util.concurrent.TimeUnit

class CountDownLatch(val initialCount: Int) {
  val underlying: _root_.java.util.concurrent.CountDownLatch = new java.util.concurrent.CountDownLatch(initialCount)
  def count: _root_.scala.Long = underlying.getCount
  def isZero: _root_.scala.Boolean = count == 0
  def countDown(): _root_.scala.Unit = underlying.countDown()
  def await(): _root_.scala.Unit = underlying.await()
  def await(timeout: Duration): _root_.scala.Boolean = underlying.await(timeout.inMillis, TimeUnit.MILLISECONDS)
  def within(timeout: Duration): _root_.scala.Boolean = await(timeout) || {
    throw new TimeoutException(timeout.toString)
  }
}

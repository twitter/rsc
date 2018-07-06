package com.twitter.util
import java.util.Collection
import java.util.concurrent.atomic.AtomicReference

/**
 * An implementation of java.util.Queue that has LIFO order and a maximum capacity of 1
 * When the Queue is full, a push replaces the item.
 */
class LastWriteWinsQueue[A] extends java.util.Queue[A] {
  val item: _root_.java.util.concurrent.atomic.AtomicReference[_root_.scala.Option[A]] = new AtomicReference[Option[A]](None)

  def clear(): Unit = {
    item.set(None)
  }

  def retainAll(p1: Collection[_]): _root_.scala.Nothing = throw new UnsupportedOperationException

  def removeAll(p1: Collection[_]): _root_.scala.Nothing = throw new UnsupportedOperationException

  def addAll(p1: Collection[_ <: A]): _root_.scala.Nothing = throw new UnsupportedOperationException

  def containsAll(p1: Collection[_]): _root_.scala.Boolean =
    p1.size == 1 && item.get == p1.iterator.next()

  def remove(candidate: AnyRef): _root_.scala.Boolean = {
    val contained = item.get
    val containsCandidate = contained.map(_ == candidate).getOrElse(false)
    if (containsCandidate) {
      item.compareAndSet(contained, None)
    }
    containsCandidate
  }

  def toArray[T](array: Array[T with java.lang.Object]): Array[T with java.lang.Object] = {
    val contained = item.get
    if (contained.isDefined && array.length > 0) {
      array(0) = contained.get.asInstanceOf[T with java.lang.Object]
      array
    } else if (contained.isDefined) {
      Array[Any](contained.get).asInstanceOf[Array[T with java.lang.Object]]
    } else Array[Any]().asInstanceOf[Array[T with java.lang.Object]]
  }

  def toArray: _root_.scala.Array[_root_.java.lang.Object] = toArray(new Array[AnyRef](0))

  def iterator: _root_.scala.Null = null

  def contains(p1: AnyRef): _root_.scala.Boolean = false

  def isEmpty: _root_.scala.Boolean = item.get.isDefined

  def size: _root_.scala.Int = if (item.get.isDefined) 1 else 0

  def peek: A = item.get.getOrElse(null.asInstanceOf[A])

  def element: A = item.get.getOrElse(throw new NoSuchElementException)

  def poll: A = item.getAndSet(None).getOrElse(null.asInstanceOf[A])

  def remove: A = item.getAndSet(None).getOrElse(throw new NoSuchElementException)

  def offer(p1: A): _root_.scala.Boolean = {
    item.set(Some(p1))
    true
  }

  def add(p1: A): _root_.scala.Boolean = {
    item.set(Some(p1))
    true
  }
}

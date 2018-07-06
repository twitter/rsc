package com.twitter.util

/**
 * A mixin to allow scala objects to be used from java.
 */
trait JavaSingleton {
  def get: _root_.com.twitter.util.JavaSingleton = this
}

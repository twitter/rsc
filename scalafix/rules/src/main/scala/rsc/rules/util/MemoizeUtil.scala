package rsc.rules.util

trait MemoizeUtil {

  /**
   * Memoize the results of a function call to prevent repeated calls
   */
  def memoize[I, O](f: I => O): I => O = {
    val _cache = new java.util.HashMap[I, O]

    key =>
      _cache.computeIfAbsent(key, _ => f(key))
  }
}

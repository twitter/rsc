/*
rules = "scala:rsc.rules.RscCompat"
 */
package com.twitter.conversions

@deprecated("Use `com.twitter.conversions.U64Ops`", "2018-12-05")
object u64 {

  /**
   * Parses this HEX string as an unsigned 64-bit long value. Be careful, this can throw
   * [[NumberFormatException]].
   *
   * @see [[java.lang.Long.parseUnsignedLong()]]
   */
  implicit class StringOps(val self: String) extends AnyVal {
    def toU64Long: Long = java.lang.Long.parseUnsignedLong(self, 16)
  }

  /**
   * Converts this unsigned 64-bit long value into a 16-character HEX string.
   */
  implicit class LongOps(val self: Long) extends AnyVal {
    def toU64HexString: String = "%016x".format(self)
  }
}

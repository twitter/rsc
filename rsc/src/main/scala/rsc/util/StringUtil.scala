// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.util

trait StringUtil {
  def stripExtraTrailingZeros(str: String): String = {
    val startExtra = str.lastIndexOf('.') + 2
    var endExtra = startExtra
    while (endExtra < str.length) {
      if (str(endExtra) == '0') endExtra += 1
    }
    if (endExtra == str.length) str.substring(0, startExtra) else str
  }
}

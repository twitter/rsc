// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc

import scala.util._

package object tests extends DumpUtil {
  type ToolResult[T] = Either[List[String], T]
}

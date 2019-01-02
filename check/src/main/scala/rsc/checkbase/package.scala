// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc

import scala.util._

package object checkbase extends DumpUtil {
  type ToolResult[T] = Either[List[String], T]
}

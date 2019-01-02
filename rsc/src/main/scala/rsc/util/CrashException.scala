// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.util

import rsc.input._

final case class CrashException(pos: Position, message: String, cause: Throwable)
    extends Exception(message, cause)

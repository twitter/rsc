// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.util

import rsc.inputs._

final case class CrashException(pos: Position, message: String, cause: Throwable)
    extends Exception(message, cause)

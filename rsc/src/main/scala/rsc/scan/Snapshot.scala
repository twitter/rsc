// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.scan

import rsc.lexis._

final case class Snapshot(
    offset: Offset,
    start: Offset,
    end: Offset,
    token: Token,
    value: Any)

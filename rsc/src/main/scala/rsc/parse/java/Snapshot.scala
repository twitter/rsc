// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse.java

import rsc.input._
import rsc.lexis.java._
import rsc.scan.java.{Snapshot => ScanSnapshot}

final case class Snapshot(
    scanner: ScanSnapshot,
    lastOffset: Offset,
    offset: Offset,
    token: Token,
    value: String)

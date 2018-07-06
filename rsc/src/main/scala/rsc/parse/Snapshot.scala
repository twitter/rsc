// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse

import rsc.lexis._
import rsc.scan.{Snapshot => ScanSnapshot}
import rsc.syntax._

final case class Snapshot(
    scanner: ScanSnapshot,
    oneTokenBehind: Boolean,
    overrideToken: Token,
    lastOffset: Offset,
    offset: Offset,
    token: Token,
    value: String,
    termWildcards: List[TermWildcard],
    tptWildcards: List[TptWildcard])

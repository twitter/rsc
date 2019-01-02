// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
// NOTE: This file has been partially copy/pasted from scalameta/scalameta
package rsc.semanticdb

sealed trait LinkMode
case object SymlinkChildren extends LinkMode
case object HardlinkChildren extends LinkMode

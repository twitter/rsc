// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package scala.meta.mjar

sealed trait Abi
case object Scalac211 extends Abi
case object Scalac212 extends Abi

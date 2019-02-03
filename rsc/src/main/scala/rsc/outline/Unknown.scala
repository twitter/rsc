// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.outline

final class Unknown private () extends Work

object Unknown {
  def apply(): Unknown = {
    new Unknown()
  }
}

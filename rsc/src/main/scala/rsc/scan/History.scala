// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.scan

trait History {
  self: Scanner =>

  def snapshot(): Snapshot = {
    Snapshot(
      offset,
      start,
      end,
      token,
      value,
      _mode,
      _ilevels,
      _ilevel,
      _slevels,
      _slevel,
      _blevel)
  }

  def restore(snapshot: Snapshot): Unit = {
    this.offset = snapshot.offset
    this.start = snapshot.start
    this.end = snapshot.end
    this.token = snapshot.token
    this.value = snapshot.value
    this._mode = snapshot._mode
    this._ilevels = snapshot._ilevels
    this._ilevel = snapshot._ilevel
    this._slevels = snapshot._slevels
    this._slevel = snapshot._slevel
    this._blevel = snapshot._blevel
  }
}

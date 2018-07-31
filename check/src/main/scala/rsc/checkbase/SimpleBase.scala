// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.checkbase

trait SimpleBase[S <: SettingsBase, N, R] extends MainBase[S, S, N, R] {
  def inputs(settings: S) = {
    List(settings)
  }

  def nscResult(settings: S, input: S): Either[List[String], N] = {
    nscResult(settings)
  }

  def nscResult(settings: S): Either[List[String], N]

  def rscResult(settings: S, input: S): Either[List[String], R] = {
    rscResult(settings)
  }

  def rscResult(settings: S): Either[List[String], R]
}

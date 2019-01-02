// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse.scala

trait Contexts {
  self: Parser =>

  sealed trait Location
  case object InBlock extends Location
  case object InTemplate extends Location
  case object Elsewhere extends Location

  sealed trait ParamContext {
    def allowsAnonymousParams: Boolean
    def allowsAnonymousTypeParams: Boolean
    def allowsContextBounds: Boolean
    def allowsDefaults: Boolean
    def allowsInferred: Boolean
    def allowsParams: Boolean
    def allowsTypeBounds: Boolean
    def allowsTypeParams: Boolean
    def allowsVariance: Boolean
    def allowsViewBounds: Boolean
  }

  case object DefnClassContext extends ParamContext {
    def allowsAnonymousParams = false
    def allowsAnonymousTypeParams = true
    def allowsContextBounds = true
    def allowsDefaults = true
    def allowsInferred = false
    def allowsParams = true
    def allowsTypeBounds = true
    def allowsTypeParams = true
    def allowsVariance = true
    def allowsViewBounds = true
  }

  case object DefnTraitContext extends ParamContext {
    def allowsAnonymousParams = false
    def allowsAnonymousTypeParams = true
    def allowsContextBounds = false
    def allowsDefaults = true
    def allowsInferred = false
    def allowsParams = false
    def allowsTypeBounds = true
    def allowsTypeParams = true
    def allowsVariance = true
    def allowsViewBounds = false
  }

  case object DefnDefContext extends ParamContext {
    def allowsAnonymousParams = false
    def allowsAnonymousTypeParams = true
    def allowsContextBounds = true
    def allowsDefaults = true
    def allowsInferred = false
    def allowsParams = true
    def allowsTypeBounds = true
    def allowsTypeParams = true
    def allowsVariance = false
    def allowsViewBounds = true
  }

  case object DefnTypeContext extends ParamContext {
    def allowsAnonymousParams = false
    def allowsAnonymousTypeParams = true
    def allowsContextBounds = false
    def allowsDefaults = false
    def allowsInferred = false
    def allowsParams = false
    def allowsTypeBounds = true
    def allowsTypeParams = true
    def allowsVariance = true
    def allowsViewBounds = false
  }

  case object CtorContext extends ParamContext {
    def allowsAnonymousParams = false
    def allowsAnonymousTypeParams = false
    def allowsContextBounds = false
    def allowsDefaults = true
    def allowsInferred = false
    def allowsParams = true
    def allowsTypeBounds = false
    def allowsTypeParams = false
    def allowsVariance = false
    def allowsViewBounds = false
  }

  case object TypeParamContext extends ParamContext {
    def allowsAnonymousParams = false
    def allowsAnonymousTypeParams = true
    def allowsContextBounds = false
    def allowsDefaults = false
    def allowsInferred = false
    def allowsParams = false
    def allowsTypeBounds = true
    def allowsTypeParams = true
    def allowsVariance = true
    def allowsViewBounds = false
  }
}

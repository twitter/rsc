// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.parse

trait Contexts {
  self: Parser =>

  sealed trait ParamContext {
    def allowsAnonymous: Boolean
    def allowsContextBounds: Boolean
    def allowsDefaults: Boolean
    def allowsInferred: Boolean
    def allowsTermParams: Boolean
    def allowsTypeBounds: Boolean
    def allowsTypeParams: Boolean
    def allowsVariance: Boolean
    def allowsViewBounds: Boolean
  }

  sealed trait TemplateContext

  final case object DefnClassContext extends ParamContext with TemplateContext {
    def allowsAnonymous = false
    def allowsContextBounds = true
    def allowsDefaults = true
    def allowsInferred = false
    def allowsTermParams = true
    def allowsTypeBounds = true
    def allowsTypeParams = true
    def allowsVariance = true
    def allowsViewBounds = true
  }

  final case object DefnObjectContext extends TemplateContext

  final case object DefnTraitContext extends ParamContext with TemplateContext {
    def allowsAnonymous = false
    def allowsContextBounds = false
    def allowsDefaults = true
    def allowsInferred = false
    def allowsTermParams = true
    def allowsTypeBounds = true
    def allowsTypeParams = true
    def allowsVariance = true
    def allowsViewBounds = false
  }

  final case object DefnDefContext extends ParamContext {
    def allowsAnonymous = false
    def allowsContextBounds = true
    def allowsDefaults = true
    def allowsInferred = false
    def allowsTermParams = true
    def allowsTypeBounds = true
    def allowsTypeParams = false
    def allowsVariance = false
    def allowsViewBounds = true
  }

  final case object DefnTypeContext extends ParamContext {
    def allowsAnonymous = true
    def allowsContextBounds = false
    def allowsDefaults = false
    def allowsInferred = false
    def allowsTermParams = false
    def allowsTypeBounds = true
    def allowsTypeParams = true
    def allowsVariance = true
    def allowsViewBounds = false
  }

  final case object PrimaryCtorContext extends ParamContext {
    def allowsAnonymous = false
    def allowsContextBounds = false
    def allowsDefaults = true
    def allowsInferred = false
    def allowsTermParams = true
    def allowsTypeBounds = false
    def allowsTypeParams = false
    def allowsVariance = false
    def allowsViewBounds = false
  }

  final case object TermNewContext extends TemplateContext
}

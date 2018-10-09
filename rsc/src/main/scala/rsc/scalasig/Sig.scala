// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.scalasig

import scala.meta.internal.{semanticdb => s}

sealed trait Sig
sealed trait NonvalueSig extends Sig
case object NoSig extends NonvalueSig
case class PolySig(stparams: s.Scope, ssig: Sig) extends NonvalueSig
case class ClassSig(sparents: List[s.Type], ssym: String) extends NonvalueSig
case class RefinedSig(sparents: List[s.Type], ssym: String) extends NonvalueSig
case class NullaryMethodSig(stpe: s.Type) extends NonvalueSig
case class MethodSig(sparams: s.Scope, stpe: s.Type) extends NonvalueSig
case class NaryMethodSig(sparams: s.Scope, ssig: Sig) extends NonvalueSig
case class TypeSig(slo: s.Type, shi: s.Type) extends NonvalueSig
case class ValueSig(stpe: s.Type) extends Sig

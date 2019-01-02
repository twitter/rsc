// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.pretty

import rsc.syntax._

trait Weights {
  sealed abstract class Weight(val value: Int)
  case object Undefined extends Weight(Int.MaxValue)

  case object ParamTyp extends Weight(0)
  case object Typ extends Weight(20)
  case object AnyInfixTyp extends Weight(40)
  case class InfixTyp(op: NamedId) extends Weight(60)
  case class RhsInfixTyp(op: NamedId) extends Weight(60)
  case object RefineTyp extends Weight(80)
  case object WithTyp extends Weight(100)
  case object AnnotTyp extends Weight(120)
  case object SimpleTyp extends Weight(140)
  case object Expr extends Weight(200)
  case object Expr1 extends Weight(220)
  case object PostfixExpr extends Weight(240)
  case class InfixExpr(op: NamedId) extends Weight(260)
  case class RhsInfixExpr(op: NamedId) extends Weight(260)
  case object PrefixExpr extends Weight(280)
  case object SimpleExpr extends Weight(300)
  case object SimpleExpr1 extends Weight(320)
  case object Pat extends Weight(400)
  case object Pat1 extends Weight(420)
  case object Pat2 extends Weight(440)
  case object AnyPat3 extends Weight(460)
  case class InfixPat(op: NamedId) extends Weight(480)
  case class RhsInfixPat(op: NamedId) extends Weight(480)
  case object SimplePat extends Weight(500)

  implicit class WeightTreeOps(tree: Tree) {
    def weight: Weight = {
      tree match {
        case _: PatAlternative => Pat
        case _: PatBind => Pat2
        case _: PatExtract => SimplePat
        case tree: PatExtractInfix => InfixPat(tree.op)
        case _: PatId => SimplePat
        case _: PatInterpolate => SimplePat
        case _: PatLit => SimplePat
        case _: PatRepeat => Pat2
        case _: PatSelect => SimplePat
        case _: PatTuple => SimplePat
        case PatVar(_, _, None) => SimplePat
        case PatVar(_, _, Some(_)) => Pat1
        case _: PatXml => SimplePat
        case _: TermAnnotate => Expr1
        case _: TermApply => SimpleExpr1
        case tree: TermApplyInfix => InfixExpr(tree.op)
        case _: TermApplyPostfix => PostfixExpr
        case _: TermApplyPrefix => PrefixExpr
        case _: TermApplyType => SimpleExpr1
        case _: TermAscribe => Expr1
        case _: TermAssign => Expr1
        case _: TermBlock => SimpleExpr
        case _: TermDo => Expr1
        case _: TermEta => PrefixExpr
        case _: TermFor => Expr1
        case _: TermForYield => Expr1
        case _: TermFunction => Expr
        case _: TermId => SimpleExpr1
        case _: TermIf => Expr1
        case _: TermInterpolate => SimpleExpr1
        case _: TermLit => SimpleExpr1
        case _: TermMatch => Expr1
        case _: TermNew => SimpleExpr
        case _: TermNewAnonymous => SimpleExpr
        case _: TermPartialFunction => SimpleExpr
        case _: TermRepeat => PostfixExpr
        case _: TermReturn => Expr1
        case _: TermSelect => SimpleExpr1
        case _: TermStub => SimpleExpr1
        case _: TermSuper => SimpleExpr1
        case _: TermThis => SimpleExpr1
        case _: TermThrow => Expr1
        case _: TermTry => Expr1
        case _: TermTryWithHandler => Expr1
        case _: TermTuple => SimpleExpr1
        case _: TermWhile => Expr1
        case _: TermWildcard => SimpleExpr1
        case _: TermWildcardFunction => Expr1
        case _: TermXml => SimpleExpr1
        case _: TptAnnotate => AnnotTyp
        case _: TptArray => SimpleTyp
        case _: TptBoolean => SimpleTyp
        case _: TptByName => ParamTyp
        case _: TptByte => SimpleTyp
        case _: TptChar => SimpleTyp
        case _: TptDouble => SimpleTyp
        case _: TptExistential => Typ
        case _: TptFloat => SimpleTyp
        case _: TptFunction => Typ
        case _: TptId => SimpleTyp
        case _: TptInt => SimpleTyp
        case _: TptIntersect => InfixTyp(TptId("&"))
        case _: TptLong => SimpleTyp
        case _: TptParameterize => SimpleTyp
        case tree: TptParameterizeInfix => InfixTyp(tree.op)
        case _: TptProject => SimpleTyp
        case _: TptRefine => RefineTyp
        case _: TptRepeat => ParamTyp
        case _: TptSelect => SimpleTyp
        case _: TptShort => SimpleTyp
        case _: TptSingleton => SimpleTyp
        case _: TptTuple => SimpleTyp
        case _: TptVoid => SimpleTyp
        case _: TptWildcard => SimpleTyp
        case _: TptWildcardExistential => SimpleTyp
        case _: TptWith => WithTyp
        case _ => Undefined
      }
    }
  }
}

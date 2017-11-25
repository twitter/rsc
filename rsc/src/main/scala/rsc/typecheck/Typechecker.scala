// Copyright (c) 2017 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.typecheck

import rsc.lexis._
import rsc.report._
import rsc.semantics._
import rsc.settings._
import rsc.syntax._
import rsc.util._

final class Typechecker private (
    settings: Settings,
    reporter: Reporter,
    symtab: Symtab) {
  def apply(env: Env, tree: Typeable): Type = {
    tree match {
      case tree: Init => init(env, tree)
      case tree: TermApply => termApply(env, tree)
      case tree: TermApplyInfix => termApplyInfix(env, tree)
      case tree: TermApplyPrefix => termApplyPrefix(env, tree)
      case tree: TermApplyType => termApplyType(env, tree)
      case tree: TermAssign => termAssign(env, tree)
      case tree: TermBlock => termBlock(env, tree)
      case tree: TermFunction => termFunction(env, tree)
      case tree: TermId => termId(env, tree)
      case tree: TermIf => termIf(env, tree)
      case tree: TermLit => termLit(env, tree)
      case tree: TermMatch => termMatch(env, tree)
      case tree: TermNew => termNew(env, tree)
      case tree: TermReturn => termReturn(env, tree)
      case tree: TermSelect => termSelect(env, tree)
      case tree: TermSuper => termSuper(env, tree)
      case tree: TermThis => termThis(env, tree)
      case tree: TermThrow => termThrow(env, tree)
      case tree: TermWhile => termWhile(env, tree)
      case tree: TptApply => tptApply(env, tree)
      case tree: TptId => tptId(env, tree)
      case tree: TptSelect => tptSelect(env, tree)
      case _ => unreachable(tree)
    }
  }

  private def init(env: Env, tree: Init): Type = {
    tree.args.foreach(apply(env, _))
    apply(env, tree.tpt)
  }

  private def termApply(env: Env, tree: TermApply): Type = {
    val funTpe = apply(env, tree.fun)
    tree.args.foreach(apply(env, _))
    funTpe match {
      case NoType =>
        NoType
      case MethodType(Nil, _, ret) =>
        ret
      case MethodType(other, _, _) =>
        unsupported("type inference")
      case other =>
        val id1 = TermId("apply").withPos(tree.fun.pos.end, tree.fun)
        val select1 = TermSelect(tree.fun, id1).withPos(tree.fun)
        val tree1 = TermApply(select1, tree.args).withPos(tree)
        apply(env, tree1)
    }
  }

  private def termApplyInfix(env: Env, tree: TermApplyInfix): Type = {
    if (tree.op.value.isLeftAssoc) {
      val select = TermSelect(tree.lhs, tree.op).withPos(tree.lhs, tree.op)
      val applyType = {
        if (tree.targs.isEmpty) select
        else TermApplyType(select, tree.targs).withPos(select, tree.targs.last)
      }
      val tree1 = TermApply(applyType, List(tree.rhs)).withPos(tree)
      apply(env, tree1)
    } else {
      unreachable(tree)
    }
  }

  private def termApplyPrefix(env: Env, tree: TermApplyPrefix): Type = {
    val arg1 = TermSelect(tree.arg, tree.op).withPos(tree)
    val tree1 = TermApply(arg1, Nil).withPos(tree)
    apply(env, tree1)
  }

  private def termApplyType(env: Env, tree: TermApplyType): Type = {
    val funTpe = apply(env, tree.fun)
    val targs = tree.targs.map { tpt =>
      apply(env, tpt) match {
        case NoType => return NoType
        case tpe: SimpleType => tpe
        case other => unreachable(other)
      }
    }
    funTpe match {
      case NoType =>
        NoType
      case MethodType(tparams, paramss, ret) =>
        funTpe.subst(tparams, targs)
      case other =>
        val id1 = TermId("apply").withPos(tree.fun.pos.end, tree.fun)
        val select1 = TermSelect(tree.fun, id1).withPos(tree.fun)
        val tree1 = TermApplyType(select1, tree.targs).withPos(tree)
        apply(env, tree1)
    }
  }

  private def termAssign(env: Env, tree: TermAssign): Type = {
    tree.lhs match {
      case TermApply(fun, args) =>
        val id1 = TermId("update").withPos(tree.lhs.pos.end, tree.lhs)
        val select1 = TermSelect(fun, id1).withPos(tree.lhs)
        val tree1 = TermApply(select1, args :+ tree.rhs).withPos(tree)
        apply(env, tree1)
      case other =>
        apply(env, other)
        apply(env, tree.rhs)
    }
  }

  private def termBlock(env: Env, tree: TermBlock): Type = {
    tree.stats match {
      case stats :+ (term: Term) =>
        val tpts = List.newBuilder[Tpt]
        val terms = List.newBuilder[Term]
        val scope = FlatScope("block")
        stats.foreach {
          case stat @ DefnField(_, id, tpt, Some(rhs)) =>
            val uid = scope.uid + id.sid.str
            scope.enter(id.sid, uid) match {
              case NoUid =>
                id.uid = uid
                symtab.outlines(id.uid) = stat
              case existingUid =>
                reporter.append(DoubleDef(stat, symtab.outlines(existingUid)))
                return NoType
            }
            tpts += tpt
            terms += rhs
          case term: Term =>
            terms += term
          case other =>
            unreachable(other)
        }
        scope.succeed()
        val env1 = scope :: env
        tpts.result.foreach(apply(env1, _))
        terms.result.foreach(apply(env1, _))
        apply(env1, term)
      case Nil =>
        SimpleType("_root_.scala.Unit#", Nil)
      case other =>
        unreachable(other)
    }
  }

  private def termFunction(env: Env, tree: TermFunction): Type = {
    val tpts = List.newBuilder[Tpt]
    val scope = FlatScope("lambda")
    tree.params.foreach {
      case param @ TermParam(_, id, tpt) =>
        val uid = scope.uid + id.sid.str
        scope.enter(id.sid, uid) match {
          case NoUid =>
            id.uid = uid
            symtab.outlines(id.uid) = param
          case existingUid =>
            reporter.append(DoubleDef(param, symtab.outlines(existingUid)))
            return NoType
        }
        tpts += tpt
    }
    scope.succeed()
    val env1 = scope :: env
    tpts.result.foreach(apply(env, _))
    apply(env1, tree.body)
  }

  private def termId(env: Env, tree: TermId): Type = {
    env.lookup(tree.sid) match {
      case NoUid =>
        reporter.append(UnboundId(tree))
        NoType
      case uid =>
        tree.uid = uid
        uid.tpe
    }
  }

  private def termIf(env: Env, tree: TermIf): Type = {
    apply(env, tree.cond)
    tree.elsep match {
      case Some(elsep) =>
        val thenTpe = apply(env, tree.thenp)
        val elseTpe = apply(env, elsep)
        lub(List(thenTpe, elseTpe))
      case None =>
        apply(env, tree.thenp)
        SimpleType("_root_.scala.Unit#", Nil)
    }
  }

  private def termLit(env: Env, tree: TermLit): Type = {
    tree.value match {
      case _: Unit => SimpleType("_root_.scala.Unit#", Nil)
      case _: Boolean => SimpleType("_root_.scala.Boolean#", Nil)
      case _: Byte => SimpleType("_root_.scala.Byte#", Nil)
      case _: Short => SimpleType("_root_.scala.Short#", Nil)
      case _: Char => SimpleType("_root_.scala.Char#", Nil)
      case _: Int => SimpleType("_root_.scala.Int#", Nil)
      case _: Float => SimpleType("_root_.scala.Float#", Nil)
      case _: Long => SimpleType("_root_.scala.Long#", Nil)
      case _: Double => SimpleType("_root_.scala.Double#", Nil)
      case _: String => SimpleType("_root_.java.lang.String#", Nil)
      case null => SimpleType("_root_.scala.AnyRef#", Nil)
      case other => unreachable(other.getClass.toString)
    }
  }

  private def termMatch(env: Env, tree: TermMatch): Type = {
    val termTpe = apply(env, tree.term)
    val caseTpes = List.newBuilder[Type]
    tree.cases.foreach {
      case caseDef @ Case(pat, cond, stats) =>
        val scope = FlatScope("case")
        def loop(pat: Pat): Unit = {
          pat match {
            case pat: PatAlternative =>
              pat.pats.foreach(loop)
            case pat: PatId =>
              val tree1 = TermId(pat.value).withPos(pat)
              apply(env, tree1)
              pat.uid = tree1.uid
            case pat: PatLit =>
              ()
            case pat: PatSelect =>
              val tree1 = TermSelect(pat.qual, pat.id).withPos(pat)
              apply(env, tree1)
            case pat @ PatVar(id: NamedId, tpt) =>
              tpt match {
                case Some(tpt) =>
                  val uid = scope.uid + id.sid.str
                  scope.enter(id.sid, uid) match {
                    case NoUid =>
                      id.uid = uid
                      symtab.outlines(id.uid) = pat
                      pat.tpe = apply(env, tpt)
                    case existingUid =>
                      val message = DoubleDef(pat, symtab.outlines(existingUid))
                      reporter.append(message)
                  }
                case None =>
                  unsupported("type inference")
              }
            case PatVar(AnonId(), _) =>
              ()
            case pat =>
              unsupported("advanced patterns")
          }
        }
        loop(pat)
        scope.succeed()
        val env1 = scope :: env
        cond.foreach(apply(env1, _))
        val stats1 = TermBlock(stats).withPos(tree)
        caseTpes += apply(env1, stats1)
    }
    lub(caseTpes.result)
  }

  private def termNew(env: Env, tree: TermNew): Type = {
    apply(env, tree._init)
  }

  private def termReturn(env: Env, tree: TermReturn): Type = {
    tree.term.foreach(apply(env, _))
    SimpleType("_root_.scala.Nothing#", Nil)
  }

  // NOTE: termSelect contains an ad hoc informally-specified bug-ridden
  // slow implementation of asSeenFrom. It's so bad that we even had to
  // add a stub method Stack.get in Parser.scala to get things going.
  // However, this allowed us to typecheck re2s, so we're going to keep
  // this monstrosity alive for the time being.
  private def termSelect(env: Env, tree: TermSelect): Type = {
    val qualTpe = apply(env, tree.qual)
    qualTpe match {
      case NoType =>
        NoType
      case qualTpe: MethodType =>
        reporter.append(NonValue(tree.qual, qualTpe))
        NoType
      case qualTpe: SimpleType =>
        def lookup(qualUid: Uid): Type = {
          val qualScope = symtab.scopes(qualUid)
          qualScope.lookup(tree.id.sid) match {
            case NoUid =>
              if (tree.id.value.isOpAssignment) {
                val value1 = tree.id.value.stripSuffix("=")
                val id1 = TermId(value1).withPos(tree.id)
                val tree1 = TermSelect(tree.qual, id1).withPos(tree)
                apply(env, tree1)
              } else {
                reporter.append(UnboundMember(qualUid, tree.id))
                NoType
              }
            case uid =>
              tree.id.uid = uid
              uid.tpe
          }
        }
        def loop(qualTpe: Type): Type = {
          qualTpe match {
            case NoType =>
              NoType
            case _: MethodType =>
              unreachable(qualTpe)
            case SimpleType(qualUid, targs) =>
              symtab.outlines(qualUid) match {
                case DefnPackage(pid, _) =>
                  lookup(pid.id.uid)
                case DefnTemplate(_, id, tparams, _, _, _) =>
                  lookup(id.uid).subst(tparams, targs)
                case DefnType(_, _, tparams, tpt) =>
                  loop(tpt.tpe.subst(tparams, targs))
                case tparam: TypeParam =>
                  loop(tparam.hi.tpe)
              }
          }
        }
        loop(qualTpe)
    }
  }

  private def termSuper(env: Env, tree: TermSuper): Type = {
    env.lookupThis(tree.qual.sidopt) match {
      case NoUid =>
        reporter.append(UnboundId(tree.qual))
        NoType
      case qualUid =>
        tree.qual.uid = qualUid
        val env1 = Env(symtab.scopes(qualUid))
        env1.lookupSuper(tree.mix.sidopt) match {
          case NoUid =>
            reporter.append(UnboundId(tree.mix))
            NoType
          case mixUid =>
            tree.mix.uid = mixUid
            symtab.outlines(mixUid) match {
              case DefnTemplate(_, id, tparams, _, _, _) =>
                val targs = tparams.map(tp => SimpleType(tp.id.uid, Nil))
                SimpleType(id.uid, targs)
              case other =>
                unreachable(other)
            }
        }
    }
  }

  private def termThis(env: Env, tree: TermThis): Type = {
    env.lookupThis(tree.qual.sidopt) match {
      case NoUid =>
        reporter.append(UnboundId(tree.id))
        NoType
      case qualUid =>
        tree.qual.uid = qualUid
        symtab.outlines(qualUid) match {
          case DefnTemplate(_, id, tparams, _, _, _) =>
            val targs = tparams.map(tparam => SimpleType(tparam.id.uid, Nil))
            SimpleType(id.uid, targs)
          case other =>
            unreachable(other)
        }
    }
  }

  private def termThrow(env: Env, tree: TermThrow): Type = {
    apply(env, tree.term)
    SimpleType("_root_.scala.Nothing#", Nil)
  }

  private def termWhile(env: Env, tree: TermWhile): Type = {
    apply(env, tree.cond)
    apply(env, tree.body)
  }

  private def tptApply(env: Env, tree: TptApply): Type = {
    val funTpe = apply(env, tree.fun)
    funTpe match {
      case NoType =>
        NoType
      case SimpleType(funUid, Nil) =>
        val targs = tree.targs.map {
          apply(env, _) match {
            case targ: SimpleType => targ
            case other => unreachable(other)
          }
        }
        SimpleType(funUid, targs)
      case other =>
        unreachable(other)
    }
  }

  private def tptId(env: Env, tree: TptId): Type = {
    env.lookup(tree.sid) match {
      case NoUid =>
        reporter.append(UnboundId(tree))
        NoType
      case uid =>
        tree.uid = uid
        SimpleType(uid, Nil)
    }
  }

  private def tptSelect(env: Env, tree: TptSelect): Type = {
    val qualTpe = apply(env, tree.qual)
    qualTpe match {
      case NoType =>
        NoType
      case SimpleType(qualUid, Nil) =>
        val qualScope = symtab.scopes(qualUid)
        qualScope.lookup(tree.id.sid) match {
          case NoUid =>
            reporter.append(UnboundMember(qualUid, tree.id))
            NoType
          case uid =>
            tree.id.uid = uid
            SimpleType(tree.id.uid, Nil)
        }
      case other =>
        unreachable(other)
    }
  }

  private implicit class TypecheckerTreeOps[T <: Tree](tree: T) {
    def withPos(startEnd: Tree): T = {
      tree.withPos(startEnd, startEnd)
    }

    def withPos(start: Tree, end: Tree): T = {
      tree.withPos(start.pos.input, start.pos.start, end.pos.end)
    }

    def withPos(start: Offset, end: Tree): T = {
      tree.withPos(end.pos.input, start, end.pos.end)
    }

    def withPos(start: Tree, end: Offset): T = {
      tree.withPos(start.pos.input, start.pos.start, end)
    }

    private def withPos(input: Input, start: Offset, end: Offset): T = {
      val syntheticPos = Position(input, start, end)
      if (tree.pos == NoPosition) {
        tree.pos = syntheticPos
        tree
      } else {
        unreachable(tree)
      }
    }
  }

  private implicit class TypecheckerTptOps(tpt: Tpt) {
    def tpe: SimpleType = {
      tpt match {
        case TptApply(fun: TptPath, targs) =>
          if (fun.id.uid == NoUid) unreachable(fun)
          else SimpleType(fun.id.uid, targs.map(_.tpe))
        case _: TptApply =>
          unreachable(tpt)
        case tpt: TptPath =>
          if (tpt.id.uid == NoUid) unreachable(tpt.id)
          else SimpleType(tpt.id.uid, Nil)
      }
    }
  }

  private implicit class TypecheckerUidOps(uid: Uid) {
    def tpe: Type = {
      symtab.outlines(uid) match {
        case DefnDef(_, _, tparams, params, ret, _) =>
          val tpeTparams = tparams.map(_.id.uid)
          val tpeParams = params.map(_.id.uid)
          val tpeRet = ret.tpe
          MethodType(tpeTparams, tpeParams, tpeRet)
        case DefnField(_, _, tpt, _) =>
          tpt.tpe
        case DefnObject(_, id, _, _) =>
          SimpleType(id.uid, Nil)
        case DefnPackage(id: TermId, _) =>
          SimpleType(id.uid, Nil)
        case DefnPackage(TermSelect(_, id: TermId), _) =>
          SimpleType(id.uid, Nil)
        case pat: PatVar =>
          pat.tpe
        case TermParam(_, _, tpt) =>
          tpt.tpe
        case outline =>
          unreachable(outline)
      }
    }
  }

  private implicit class TypecheckerTpeOps(tpe: Type) {
    def subst(tparams: List[TypeParam], targs: List[SimpleType]): Type = {
      if (tparams.isEmpty && targs.isEmpty) {
        tpe
      } else {
        tpe match {
          case NoType =>
            NoType
          case tpe: MethodType =>
            val tparams1 = tpe.tparams.diff(tparams.map(_.id.uid))
            val params1 = tpe.params
            val ret1 = {
              tpe.ret.subst(tparams, targs) match {
                case ret1: SimpleType => ret1
                case other => unreachable(other)
              }
            }
            MethodType(tparams1, params1, ret1)
          case tpe: SimpleType =>
            val i = tparams.indexWhere(_.id.uid == tpe.uid)
            if (i != -1) {
              targs(i)
            } else {
              val uid1 = tpe.uid
              val targs1 = tpe.targs.map {
                _.subst(tparams, targs) match {
                  case targ1: SimpleType => targ1
                  case other => unreachable(other)
                }
              }
              SimpleType(uid1, targs1)
            }
        }
      }
    }
    def subst(tparams: Seq[Uid], targs: List[SimpleType]): Type = {
      if (tparams.isEmpty && targs.isEmpty) {
        tpe
      } else {
        tpe match {
          case NoType =>
            NoType
          case tpe: MethodType =>
            val tparams1 = tpe.tparams.diff(tparams)
            val params1 = tpe.params
            val ret1 = {
              tpe.ret.subst(tparams, targs) match {
                case ret1: SimpleType => ret1
                case other => unreachable(other)
              }
            }
            MethodType(tparams1, params1, ret1)
          case tpe: SimpleType =>
            val i = tparams.indexWhere(_ == tpe.uid)
            if (i != -1) {
              targs(i)
            } else {
              val uid1 = tpe.uid
              val targs1 = tpe.targs.map {
                _.subst(tparams, targs) match {
                  case targ1: SimpleType => targ1
                  case other => unreachable(other)
                }
              }
              SimpleType(uid1, targs1)
            }
        }
      }
    }
  }

  // NOTE: This is a markedly incorrect implementation, but it allowed us
  // to typecheck re2s, so we're going to keep it for the time being.
  private def lub(tpes: List[Type]): Type = {
    tpes.distinct match {
      case List(tpe) => tpe
      case _ => SimpleType("_root_.scala.Any#", Nil)
    }
  }
}

object Typechecker {
  def apply(
      settings: Settings,
      reporter: Reporter,
      symtab: Symtab): Typechecker = {
    new Typechecker(settings, reporter, symtab)
  }
}

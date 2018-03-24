// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.report

import java.io._
import rsc.lexis._
import rsc.pretty._
import rsc.semantics._
import rsc.syntax._
import rsc.typecheck._
import rsc.util._

sealed trait Message extends Pretty with Product {
  def sev: Severity
  def pos: Position
  def text: String
  def explanation: String = ""
  def printStr(p: Printer): Unit = PrettyMessage.str(p, this)
  def printRepl(p: Printer): Unit = PrettyMessage.repl(p, this)
}

// ============ FUNDAMENTAL ============

final case class CrashMessage(pos: Position, message: String, ex: Throwable)
    extends Message {
  def sev = FatalSeverity
  def text = "compiler crash"
  override def explanation = {
    if (ex != null) {
      val details = new StringWriter()
      ex.printStackTrace(new PrintWriter(details))
      details.toString
    } else {
      ""
    }
  }
}

final case class VerboseMessage(pos: Position, text: String) extends Message {
  def sev = VerboseSeverity
}

object VerboseMessage {
  def apply(text: String): VerboseMessage = VerboseMessage(NoPosition, text)
}

// ============ LEXER ============

final case class IllegalCharacter(pos: Position) extends Message {
  def sev = FatalSeverity
  def text = "illegal character"
}

final case class IllegalComment(pos: Position) extends Message {
  def sev = FatalSeverity
  def text = "illegal comment"
}

final case class IllegalEscape(pos: Position) extends Message {
  def sev = FatalSeverity
  def text = "illegal escape"
}

final case class IllegalNumber(pos: Position) extends Message {
  def sev = FatalSeverity
  def text = "illegal number"
}

final case class LeadingZero(pos: Position) extends Message {
  def sev = FatalSeverity
  def text = "leading zeros not allowed"
}

final case class UnclosedCharacter(pos: Position) extends Message {
  def sev = FatalSeverity
  def text = "unclosed character literal"
}

final case class UnclosedSinglelineString(pos: Position) extends Message {
  def sev = FatalSeverity
  def text = "unclosed string literal"
}

final case class UnclosedMultilineString(pos: Position) extends Message {
  def sev = FatalSeverity
  def text = "unclosed multi-line string literal"
}

// ============ PARSER ============

final case class ExpectedToken(pos: Position, expected: Token, actual: Token)
    extends Message {
  def sev = FatalSeverity
  def text = s"${tokenStr(expected)} expected but ${tokenStr(actual)} found"
}

final case class ExpectedId(pos: Position, expected: String, actual: Token)
    extends Message {
  def sev = FatalSeverity
  def text = "$expected expected but ${tokenStr(actual)} found"
}

final case class ExpectedClassOrObjectDefinition(pos: Position)
    extends Message {
  def sev = FatalSeverity
  def text = "expected class or object definition"
}

final case class ExpectedStartOfDefinition(pos: Position) extends Message {
  def sev = FatalSeverity
  def text = "expected start of definition"
}

final case class ExpectedTypeRhs(pos: Position) extends Message {
  def sev = FatalSeverity
  def text = "expected =, >:, or <:"
}

final case class FileNotFound(input: Input) extends Message {
  def sev = ErrorSeverity
  def pos = Position(input, NoOffset, NoOffset)
  def text = s"file not found"
}

final case class FilesNotFound() extends Message {
  def sev = ErrorSeverity
  def pos = Position(NoInput, NoOffset, NoOffset)
  def text = s"nothing to compile"
}

final case class IllegalIdentifier(pos: Position) extends Message {
  def sev = FatalSeverity
  def text = "illegal identifier"
}

final case class IllegalLiteral(pos: Position) extends Message {
  def sev = FatalSeverity
  def text = "illegal literal"
}

final case class IllegalModifier(pos: Position) extends Message {
  def sev = ErrorSeverity
  def text = "illegal modifier"
}

final case class IllegalStartOfDeclaration(pos: Position) extends Message {
  def sev = FatalSeverity
  def text = "illegal start of declaration"
}

final case class IllegalStartOfDefinition(pos: Position) extends Message {
  def sev = FatalSeverity
  def text = "illegal start of definition"
}

final case class IllegalStartOfSimplePat(pos: Position) extends Message {
  def sev = FatalSeverity
  def text = "illegal start of simple pattern"
}

final case class IllegalStartOfSimpleTerm(pos: Position) extends Message {
  def sev = FatalSeverity
  def text = "illegal start of simple expression"
}

final case class IllegalStartOfStatement(pos: Position) extends Message {
  def sev = FatalSeverity
  def text = "illegal start of statement"
}

final case class MixedLeftAndRightAssociativeOps(
    pos: Position,
    op1: String,
    op2: String)
    extends Message {
  def sev = ErrorSeverity
  def text = {
    def status(op: String) = {
      if (op1.isLeftAssoc) "which is left-associative"
      else "which is right-associative"
    }
    val status1 = status(op1)
    val status2 = status(op2)
    val error = "have same precedence and may not be mixed"
    s"`$op1` ($status1) and `$op2` ($status2) $error"
  }
}

final case class RepeatedModifier(pos: Position) extends Message {
  def sev = ErrorSeverity
  def text = s"repeated modifier"
}

// ============ TYPECHECKER ============

final case class DoubleDef(tree: Outline, existing: Outline) extends Message {
  def sev = ErrorSeverity
  def pos = tree.id.point
  def text = {
    if (tree.isInstanceOf[DefnDef] || existing.isInstanceOf[DefnDef]) {
      crash("overloading")
    } else {
      val treeDesc = {
        tree.id match {
          case AnonId() => crash(tree)
          case id: NamedId => id.value
        }
      }
      val existingDesc = PrettyOutline.desc(existing)
      s"$treeDesc is already defined as $existingDesc"
    }
  }
}

final case class IllegalCyclicReference(scope: Scope) extends Message {
  def sev = ErrorSeverity
  def pos = {
    scope match {
      case scope: ImporterScope => scope.tree.point
      case scope: TemplateScope => scope.tree.point
      case _ => crash(scope)
    }
  }
  def text = {
    val CyclicStatus(cycle) = scope.status
    val description = {
      def loop(scopes: List[Scope]): String = {
        scopes match {
          case List() => crash(cycle)
          case List(only) => name(only)
          case List(prelast, last) => name(prelast) + " and " + name(last)
          case scope :: rest => name(scope) + ", " + loop(rest)
        }
      }
      loop(cycle)
    }
    s"illegal cyclic reference involving $description"
  }
  private def name(scope: Scope): String = {
    scope match {
      case scope: TemplateScope =>
        PrettyOutline.desc(scope.tree)
      case scope: ImporterScope =>
        val p = new Printer
        p.str("import ")
        p.str(scope.tree)
        p.toString
      case _ =>
        crash(scope)
    }
  }
}

final case class IllegalOutlinePart(part: Tree) extends Message {
  def sev = ErrorSeverity
  def pos = part.point
  def text = "illegal outline part"
}

final case class NonValue(term: Term, tpe: Type) extends Message {
  def sev = ErrorSeverity
  def pos = term.point
  def text = s"not a value: $tpe"
}

final case class UnboundMember(qualSym: Symbol, id: Id) extends Message {
  def sev = ErrorSeverity
  def pos = id.point
  def text = {
    val qualDesc = qualSym
    id match {
      case AnonId() => crash(id)
      case CtorId() => crash(id)
      case PatId(value) => s"unbound: value $qualDesc.$value"
      case SomeId(value) => s"unbound: $qualDesc.$value"
      case TermId(value) => s"unbound: value $qualDesc.$value"
      case TptId(value) => s"unbound: type $qualDesc.$value"
    }
  }
}

final case class UnboundId(id: Id) extends Message {
  def sev = ErrorSeverity
  def pos = id.point
  def text = {
    id match {
      case AnonId() => crash(id)
      case CtorId() => crash(id)
      case PatId(value) => s"unbound: value $value"
      case SomeId(value) => s"unbound: $value"
      case TermId(value) => s"unbound: value $value"
      case TptId(value) => s"unbound: type $value"
    }
  }
}

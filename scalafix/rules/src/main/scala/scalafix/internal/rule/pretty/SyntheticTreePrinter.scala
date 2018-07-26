package scalafix.internal.rule.pretty

import rsc.pretty.Printer
import scala.meta._
import rsc.{syntax => r}
import rsc.pretty._
import scala.meta.inputs._
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.Scala.{Descriptor => d}
import scala.meta.internal.semanticdb.Scala._
import scalafix.internal.rule.semantics.Env

class SyntheticTreePrinter(
    env: Env,
    input: Input,
    doc: s.TextDocument,
    treePositions: Map[s.Range, Tree])
    extends Printer {

  implicit class ScalametaTermOps(term: Term) {
    def rscWeight: Weight = term match {
      case term: Term.Name => r.TermId(null).weight
      case term: Term.Apply =>
        r.TermApply(null, null).weight
      case term: Term.ApplyInfix =>
        r.TermApplyInfix(r.TermId("$synth"), term.op.toRscId, List(), List())
          .weight
      case term: Term.ApplyType =>
        r.TermApplyType(null, null).weight
      case term: Lit =>
        r.TermLit(null).weight
      case term: Term.Select =>
        r.TermSelect(null, null).weight
    }
    def toRscId: r.TermId = term match {
      case Term.Name(value) => r.TermId(value)
    }
  }

  def pprint(tree: s.Tree): Unit = tree match {
    case s.OriginalTree(range) =>
      val r = range.get
      val lines = input.text.split('\n').toSeq
      val linesSubseq = lines.slice(r.startLine, r.endLine + 1)
      if (r.startLine == r.endLine) {
        str(linesSubseq.head.substring(r.startCharacter, r.endCharacter))
      } else {
        val mid = linesSubseq.tail.init
        val newFirstLine = linesSubseq.head.substring(r.startCharacter)
        val newEndLine = linesSubseq.last.substring(0, r.endCharacter)
        str((newFirstLine +: mid :+ newEndLine).mkString("\n"))
      }
    case s.ApplyTree(fn, args) =>
      pprint(fn)
      rep("(", args, ", ", ")")(t => pprint(t))
    case s.TypeApplyTree(fn, targs) =>
      pprint(fn)
      rep("[", targs, ", ", "]") { t =>
        val typePrinter = new TypePrinter(env)
        typePrinter.pprint(t)
        str(typePrinter.toString)
      }
    case s.SelectTree(qual, id) =>
      val needsParens = qual match {
        case s.OriginalTree(range) =>
          val rscWeight = treePositions(range.get).asInstanceOf[Term].rscWeight
          rscWeight.value < SimpleExpr1.value
        case _ => false
      }
      if (needsParens) str("(")
      pprint(qual)
      if (needsParens) str(")")
      str(".")
      str(id.get.sym.desc.name)
    case s.IdTree(sym) => pprintFqn(env, sym)
    case s.FunctionTree(params, term) =>
      str("{")
      params match {
        case Seq() => ()
        case Seq(id) =>
          pprintName(env, id.sym)
          str(" => ")
        case _ =>
          rep("(", params, ", ", ") => ")(id => pprintName(env, id.sym))
      }
      pprint(term)
      str("}")
    case s.MacroExpansionTree(tpe) =>
      val typePrinter = new TypePrinter(env)
      typePrinter.pprint(tpe)
      str("??? : ")
      str(typePrinter.toString)
  }

  def pprintName(env: Env, sym: String): Unit = {
    val typePrinter = new TypePrinter(env)
    doc.symbols.foreach(typePrinter.addInfo)
    typePrinter.pprint(sym)
    str(typePrinter.toString)
  }

  def pprintFqn(env: Env, sym: String): Unit = {
    val owner = sym.owner
    if (owner != Symbols.None) {
      owner.desc match {
        case _: d.Package =>
          pprintFqn(env, sym.owner)
          str(".")
        case _: d.Term =>
          pprintFqn(env, sym.owner)
          str(".")
        case desc: d.Type =>
          if (env.lookupThis(desc.name) == owner) {
            pprintName(env, owner)
            str(".this.")
          } else str(".")
      }
    }
    pprintName(env, sym)
  }

}

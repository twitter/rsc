package scalafix.internal.rule.pretty

import scala.meta._
import rsc.{syntax => r}
import rsc.pretty._
import scala.meta.internal.metap.{BasePrinter, RangePrinter}
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.Scala.{Descriptor => d}
import scala.meta.internal.semanticdb.Scala._
import scalafix.internal.rule.semantics.Env

class SyntheticTreePrinter(
    env: Env,
    doc: s.TextDocument
) extends Printer {

  private val rp = new BasePrinter(null, null, null) with RangePrinter {}
  import rp.DocumentOps

  val syms: Map[String, s.SymbolInformation] =
    doc.symbols.map(info => info.symbol -> info).toMap

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
      str(doc.substring(range).get)
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
          val originalTerm = doc.substring(range).get.parse[Term].get
          originalTerm match {
            case _: Term.ApplyInfix => true
            case _ => false
          }
        case _ => false
      }
      if (needsParens) str("(")
      pprint(qual)
      if (needsParens) str(")")
      str(".")
      str(id.get.sym.desc.name)
    case s.IdTree(sym) => pprintFqn(sym)
    case s.FunctionTree(params, term) =>
      str("{")
      params match {
        case Seq() => str("() => ")
        case Seq(id) =>
          pprintName(id.sym)
          str(" => ")
        case _ =>
          rep("(", params, ", ", ") => ")(id => pprintName(id.sym))
      }
      pprint(term)
      str("}")
    case s.MacroExpansionTree(tpe) =>
      val typePrinter = new TypePrinter(env)
      typePrinter.pprint(tpe)
      str("??? : ")
      str(typePrinter.toString)
    case _ => sys.error(s"unsupported tree $tree")
  }

  def pprintName(sym: String): Unit = syms.get(sym) match {
    case Some(info) => str(info.name)
    case None => str(sym.desc.name)
  }

  def pprintFqn(sym: String): Unit = {
    if (sym.owner != Symbols.None) {
      sym.owner.desc match {
        case _: d.Package =>
          pprintFqn(sym.owner)
          str(".")
        case _: d.Term =>
          pprintFqn(sym.owner)
          str(".")
        case desc: d.Type =>
          if (env.lookupThis(desc.name) == sym.owner) {
            pprintName(sym.owner)
            str(".this.")
          } else str(".")
        case desc => sys.error(s"unsupported desc $desc")
      }
    }
    pprintName(sym)
  }

}

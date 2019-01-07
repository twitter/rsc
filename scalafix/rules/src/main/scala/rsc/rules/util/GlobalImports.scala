package rsc.rules.util
import scala.annotation.tailrec
import scala.meta.{Dialect, Import, Importee, Importer, Pkg, Source, Stat, Token, Tree}
import scalafix.v0.{Patch, RuleCtx}

/**
 *  A rip-off of scalafix.internal.patch.ImportPatchOps
 *  This is used to circumvent the problem in which a global import
 *  fails to be added if the import happens in a more local scope.
 */
final class GlobalImports(ctx: RuleCtx) {

  def addGlobalImports(importersToAdd: Seq[Importer]): Patch = {
    val grouped: Seq[Importer] =
      importersToAdd
        .groupBy(_.ref.syntax)
        .map {
          case (_, is) =>
            Importer(
              is.head.ref,
              is.flatMap(_.importees)
                .sortBy({
                  case Importee.Name(n) => n.value
                  case Importee.Rename(n, _) => n.value
                  case Importee.Unimport(n) => n.value
                  case Importee.Wildcard() => '\uFFFF'.toString
                })
                .toList
            )
        }
        .toList
        .sortBy(_.ref.syntax)

    ctx.addRight(getGlobalImportsEditToken, importStr(grouped))
  }

  private val globalImports: Seq[Import] = getGlobalImports(ctx.tree)

  private def importStr(grouped: Seq[Importer]): String = {
    val end = if (globalImports.isEmpty) "\n" else ""

    grouped
      .map { importer =>
        val withoutSpaces = importer.syntax.replaceFirst("\\{ ", "{").replaceFirst(" \\}", "}")
        s"import $withoutSpaces"
      }
      .mkString("\n", "\n", end)
  }

  @tailrec private def getGlobalImports(ast: Tree): Seq[Import] =
    ast match {
      case Pkg(_, Seq(pkg: Pkg)) => getGlobalImports(pkg)
      case Source(Seq(pkg: Pkg)) => getGlobalImports(pkg)
      case Pkg(_, stats) => extractImports(stats)
      case Source(stats) => extractImports(stats)
      case _ => Nil
    }

  private def extractImports(stats: Seq[Stat]): Seq[Import] = {
    stats
      .takeWhile(_.is[Import])
      .collect { case i: Import => i }
  }

  private def toks(t: Tree) = t.tokens(implicitly[Dialect])

  private def fallbackToken: Token = {
    def loop(tree: Tree): Token = tree match {
      case Source((stat: Pkg) :: _) => loop(stat)
      case Source(_) => toks(tree).head
      case Pkg(_, stat :: _) => loop(stat)
      case els => ctx.tokenList.prev(ctx.tokenList.prev(toks(els).head))
    }
    loop(ctx.tree)
  }

  private def getGlobalImportsEditToken: Token = {
    if (globalImports.isEmpty) fallbackToken
    else toks(globalImports.last).last
  }
}

package rsc.checkscalasig

import better.files._
import java.nio.file.Path
import rsc.checkbase.{CheckerBase, DiffUtil, DifferentProblem}
import scala.collection.mutable
import scala.meta.scalasig.highlevel._

/**
 *
 * Checks *high-level* scalasigs
 *
 * @param settings
 * @param nscResult Scalac-produced classfile with scalasigs
 * @param rscResult Rsc-produced classfile with scalasigs
 */
class Checker(settings: Settings, nscResult: Path, rscResult: Path)
    extends CheckerBase
    with DiffUtil {

  val nscFilenameDefaultBase = "nsc_scalasigs"
  val rscFilenameDefaultBase = "rsc_scalasigs"

  private def symbols(scalasig: Scalasig): Map[Id, EmbeddedSymbol] =
    scalasig.symbols.map(sym => sym.id -> sym).toMap

  private def resultSyms(sig: ScalasigResult): Option[(String, Map[Id, EmbeddedSymbol])] = {
    sig match {
      case ParsedScalasig(_, _, scalasig) => Some(scalasig.name -> symbols(scalasig))
      case _ => None
    }
  }

  def check(): Unit = {

    val nscSigs = Scalasigs.list(nscResult).flatMap(resultSyms).toMap
    val rscSigs = Scalasigs.list(rscResult).flatMap(resultSyms).toMap

    assert(nscSigs.keySet == rscSigs.keySet)

    val nscTexts = mutable.ListBuffer.empty[String]
    val rscTexts = mutable.ListBuffer.empty[String]

    nscSigs.foreach {
      case (k, nscSyms) =>
        val rscSyms = rscSigs(k)

        val nscSymStrs = nscSyms.mapValues(_.toString)
        val rscSymStrs = rscSyms.mapValues(_.toString)

        if (settings.saveOutput) {
          nscTexts.prepend(nscSymStrs.mkString("\n"))
          rscTexts.prepend(rscSymStrs.mkString("\n"))
        }

        val relevant_ids = (nscSymStrs.keySet ++ rscSymStrs.keySet).toList.sorted

        relevant_ids.foreach { id =>
          val nscString = nscSymStrs.getOrElse(id, "")
          val rscString = rscSymStrs.getOrElse(id, "")

          if (nscString != rscString) {
            problems += DifferentProblem(s"$k: $id", nscString, rscString)
          }
        }
    }

    if (settings.saveOutput) {
      val nscFile = s"$nscFilenameDefaultBase.txt".toFile
      val rscFile = s"$rscFilenameDefaultBase.txt".toFile

      nscFile.write(nscTexts.mkString("\n"))
      rscFile.write(rscTexts.mkString("\n"))
    }

  }
}

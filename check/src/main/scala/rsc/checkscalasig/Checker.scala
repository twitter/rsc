package rsc.checkscalasig

import java.nio.file.Path
import rsc.checkbase.{CheckerBase, DiffUtil, DifferentProblem}
import scala.meta.scalasig.highlevel.{ParsedScalasig, ScalasigResult, Scalasigs}

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

  private def resStr(sig: ScalasigResult): Option[(String, String)] = {
    sig match {
      case ParsedScalasig(_, _, scalasig) => Some(scalasig.name -> scalasig.toString)
      case _ => None
    }
  }

  def check(): Unit = {
    val nscSigs = Scalasigs.list(nscResult).flatMap(resStr).toMap
    val rscSigs = Scalasigs.list(rscResult).flatMap(resStr).toMap

    assert(nscSigs.keySet == rscSigs.keySet)

    nscSigs.foreach {
      case (k, nscText) =>
        val rscText = rscSigs(k)

        val unifiedDiff = diff(nscResult.toString, nscText, rscText.toString, rscText)

        unifiedDiff.foreach { d =>
          problems += DifferentProblem(d)
        }
    }
  }
}

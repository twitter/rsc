package rsc.checkscalasig

import better.files._
import java.nio.file.Path
import rsc.checkbase.{CheckerBase, DiffUtil, DifferentProblem}
import scala.collection.mutable
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

  val nscFilenameDefaultBase = "nsc_scalasigs"
  val rscFilenameDefaultBase = "rsc_scalasigs"

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

    val nscTexts = mutable.ListBuffer.empty[String]
    val rscTexts = mutable.ListBuffer.empty[String]

    nscSigs.foreach {
      case (k, nscText) =>
        val rscText = rscSigs(k)

        if (settings.saveOutput) {
          nscTexts.prepend(nscText)
          rscTexts.prepend(rscText)
        }

        val unifiedDiff = diff(nscResult.toString, nscText, rscText.toString, rscText)

        unifiedDiff.foreach { d =>
          problems += DifferentProblem(d)
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

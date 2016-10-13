package ingester.comparer

import java.io.File
import java.nio.file.Path

import ingester.comparer.SrtFullComparer.{FullComparisonParameters, SameTextShiftedTimings, TimingShift, ZeroShift}
import ingester.dissector.SrtDissector
import ingester.dissector.Vocabulary.Srt

import scala.collection.mutable.{ListBuffer, Map => MutableMap}
import scala.util._
import ingester._
import ingester.comparer.SrtsTextualMatcher.TextualMatchingParameters
object SrtsClassifier {

  implicit val comparisonParams = FullComparisonParameters(
    TextualMatchingParameters(
      30,
      2,
      0.85
    ),
    0.85,
    1000
  )


  def classify(files: Seq[File]): Map[Srt, Map[TimingShift, Seq[Path]]] = {
    classify {
      files.flatMap { file =>
        SrtDissector.parseWithCharsetDetection(file) match {
          case Success(srt) =>
            Some(file.toPath -> srt)
          case Failure(t) =>
            warn(t.getMessage)
            None
        }
      }.toMap
    }
  }

  private def classify(pathsAndSrts: Map[Path, Srt]): Map[Srt, Map[TimingShift, Seq[Path]]] = {
    val classification = MutableMap[Srt, MutableMap[TimingShift, ListBuffer[Path]]]()
    pathsAndSrts.foreach { case (path, srt) =>
      placeNewFileInClassification(classification, srt, path)
    }
    // make immutable
    classification.toMap.mapValues(_.toMap.mapValues(_.toSeq))
  }


  private def placeNewFileInClassification(
    //the srts are regrouped by textual similarity
    //then by variation of the timings
    classification: MutableMap[Srt, MutableMap[TimingShift, ListBuffer[Path]]],
    newSrt: Srt,
    path: Path
  ): Unit = {
    classification
      // compare with each Srt already found
      .map { case (srt, rest) =>
        // add in the result of the comparison
        (srt, (SrtFullComparer.compare(srt, newSrt), rest))
      }
      // find the first with the same text and a shift
      .find {
        case (_, (_: SameTextShiftedTimings, _)) => true
        case _ => false
      } match {
        case None =>
          // brand new
          classification(newSrt) = MutableMap(ZeroShift -> ListBuffer(path))
        case Some((srt, (SameTextShiftedTimings(foundShift), mapByShifts))) =>
          mapByShifts
            .find {
            case (shift, _) => shift == foundShift
          } match {
            case None =>
              //new shift
              mapByShifts(foundShift) = ListBuffer(path)
            case Some((_, paths: ListBuffer[Path])) =>
              paths += path
          }
      }
  }

}

package ingester.comparer

import java.lang.Math._

import ingester.dissector.Vocabulary._
import org.apache.commons.lang3.StringUtils

import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._

object SrtsTextualMatcher {

  type SrtsTextualMatchingResult = Seq[(SubtitleBlock, SubtitleBlock)]

  case class TextualMatchingParameters(
    blocksToConsiderFromBeginning: Int,
    maxShiftBetweenMatchingsBlocks: Int,
    stringsSimilarityThreshold: Double
  )

  /**
   * Compares the first subtitles from two .srt files
   * and try to matches them based on an approximate
   * string comparison.
   * @param a the first .srt
   * @param b the other .srt
   * @param params some parameters for the algorithm
   * @return a sequence of pairs of matched subtitles blocks,
   *         where in each pair the first comes from a
   *         and the second comes from b.
   *         The sequence is empty if a and b had nothing in
   *         common. The more it contains, the more a and b
   *         are similar.
   */
  def computeMatches(a: Srt, b: Srt)(implicit params: TextualMatchingParameters): SrtsTextualMatchingResult = {
    //imperative style because it's just too tricky
    val aMax = min(a.length, params.blocksToConsiderFromBeginning)
    val bMax = min(b.length, params.blocksToConsiderFromBeginning)
    var bIdx = 0
    val res = ListBuffer[(SubtitleBlock, SubtitleBlock)]()
    //iterate over a
    for (aIdx <- 0 until aMax) {
      breakable {
        //iterate over the next few equivalents in b
        for (bSubIdx <- bIdx until min(bMax, bIdx + params.maxShiftBetweenMatchingsBlocks + 1)) {
          val blockA = a(aIdx)
          val blockB = b(bSubIdx)
          if(isMatch(blockA, blockB)){
            //add this pair to the result
            res += ((blockA, blockB))
            // move the main cursor on b
            bIdx = bSubIdx + 1
            //break that second iteration
            break
          }
        }
      }
    }
    res.toList
  }


  private def isMatch(blockA: SubtitleBlock, blockB: SubtitleBlock)(implicit params: TextualMatchingParameters) =
    StringUtils.getJaroWinklerDistance(
      blockA.lines.mkString,
      blockB.lines.mkString
    ) >= params.stringsSimilarityThreshold



}

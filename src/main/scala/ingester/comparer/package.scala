package ingester

import ingester.dissector.Vocabulary.Srt
import org.apache.commons.lang3.StringUtils

package object comparer extends AdsCleaningUtils {

  def firstChars(srt: Srt, nbChars: Int): String =
    srt.flatMap(_.lines).mkString("\n").take(nbChars)

  def stringsSimilarityRate(s1: String, s2: String) = {
    StringUtils.getJaroWinklerDistance(s1, s2)
  }




}

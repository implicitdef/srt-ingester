package ingester.comparer

import ingester.dissector.Vocabulary.{SubtitleBlock, _}

trait AdsCleaningUtils {

  //Removes the frequent 'ads' at the start and end of subtitles
  def withoutAds(srt: Srt): Srt =
  srt
    .zipWithIndex
    .filterNot { case (block, i) =>
      isIndexInHeadOrTail(srt, i, 50) &&
        isLikelyGarbage(block)
    }
    .map(_._1)


  private def isIndexInHeadOrTail(seq: Seq[_], index: Int, headAndTailSizes: Int) =
    index < headAndTailSizes ||
      index >= seq.length - headAndTailSizes

  private def isLikelyGarbage(b: SubtitleBlock): Boolean =
    b.lines.exists(isLikelyGarbage)

  private def isLikelyGarbage(s: String) = {
    val s2 = s.toLowerCase
    s2.contains("forom.com") ||
      s2.contains("www.addic7ed.com") ||
      s2.contains("www.tvsubtitles.net") ||
      s2.contains("www.opensubtitles.org") ||
      s2.contains("www.allsubs.org") ||
      s2.contains("uksubtitles.ru") ||
      s2.contains("open subtitles mkv player") ||
      (
        s2.contains("by") &&
          (
            s2.matches(".*sync.*") ||
              s2.matches(".*correct.*") ||
              s2.matches(".*subtitles.*") ||
              s2.matches(".*shared.*")
            )
        )
  }


}

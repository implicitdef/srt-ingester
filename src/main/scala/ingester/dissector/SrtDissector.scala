package ingester.dissector


import java.io._

import ingester._
import ingester.dissector.Vocabulary._
import org.apache.commons.io.IOUtils
import org.apache.commons.io.input.BOMInputStream

import scala.util.Try

object SrtDissector {

  def parseWithCharsetDetection(file: File): Try[Srt] = {
    val contentIfUtf8 = read(file, "UTF-8")
    val detectedCharset = EncodingDetector.detect(contentIfUtf8)
    log(s"Detected charset $detectedCharset")
    val trueContent = read(file, detectedCharset)
    SrtParsers.doFullParsing(new StringReader(trueContent))
  }

  private def read(file: File, charset: String) = {
    val is = new BOMInputStream(new FileInputStream(file))
    IOUtils.toString(is, charset)
  }

}

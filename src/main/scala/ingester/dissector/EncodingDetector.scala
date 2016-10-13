package ingester.dissector

object EncodingDetector {

  private val Chars = "âêôûÄéÆÇàèÊùÌÍÎÏÐîÒÓÔÕÖØÙÚÛÜÝßàáâãäåæçèéêëìíîïðñòóôõöùúûüýÿ"
  private val Encodings = Seq("ISO-8859-1", "ISO-8859-15", "Windows-1252")
  private val Ratio = 0.001

  private val UTF8 = "UTF-8"
  private def generateWrongChars(encodingRead: String, encodingWrite: String): Seq[String] = {
    Chars.map { char =>
      new String((char+"").getBytes(encodingRead), encodingWrite)
    }
  }

  // map that associate the encoding name with the special wrong chars that can result from a bad conversion
  private val encodingMap: Map[String, Seq[String]] = Encodings.map { encoding =>
    encoding -> (generateWrongChars(UTF8, encoding) ++ generateWrongChars(encoding, UTF8))
  }.toMap

  def detect(content: String): String = {
    // init with 0 count for each encoding
    var encodingMatches = encodingMap.map { case (enc, chars) => enc -> 0 }
    content.foldLeft("") {
      case (paire, char) if paire.length < 2 => paire + char
      case (paire, char) if paire.length == 2 =>
        val charStr = char.toString
        encodingMap.foreach { case (enc, wrongChars) =>
          if (wrongChars.contains(paire) || wrongChars.contains(charStr)) {
            encodingMatches = encodingMatches + ( enc -> (encodingMatches(enc)+1) )
          }
        }
        paire.drop(1) + char
    }
    val (encoding, matchNb) = encodingMatches.toSeq.sortBy(-_._2).head
    val ratio: Double = matchNb.toDouble / content.length.toDouble
    if (ratio > Ratio) encoding else UTF8
  }
}


package ingester

import java.io.{BufferedInputStream, File, FileInputStream}
import java.nio.file.Paths

import ingester.comparer.SrtsClassifier
import ingester.dissector.EncodingDetector
import org.apache.commons.io.FileUtils

import scala.concurrent.Future


object Hello {

  def main(args: Array[String]): Unit = {
    log("Hello")
    val engine = new Engine
    val moviesFolder = "/Users/manu/Downloads/utorrent"
    //val movieFilePath = s"$moviesFolder/La Piel Que Habito [dvdrip][spanish][AC35.1][www.lokotorrents.com]/movie.avi"
    val movieFilePath = s"$moviesFolder/Juego de Tronos - [Temp.1] [HDTV][Spanish] [Musashi Media Torrents]/Juego de Tronos - Temp.1 [HDTV][Cap.102][Spanish].avi"
    val lang = "Spanish"
    val subtitlesInfos = engine.findAllSubtitlesInfos(
      lang,
      new File(movieFilePath),
      Seq("Juego de Tronos", "Game of Thrones"),
      "1",
      "2"
    ).await()

    val downloadedFiles = Future.traverse(subtitlesInfos){ s =>
      val folder = Paths.get(movieFilePath).resolveSibling("srts").resolve(s.getId.toString).toFile
      engine.downloadToFolderAndReturnsFiles(s, folder)
    }.await().flatten

    log("-----------------")
    log("Classifiying")
    log("-----------------")
    val selectedFiles = downloadedFiles
    val classification = SrtsClassifier.classify(selectedFiles)
    classification
      .mapValues(_.values.flatten.size)
      .toSeq
      .filter(_._2 > 1)
      .sortBy(_._2)
      .foreach { case (srt, occurences) =>
      log(" > New text variant")
      log(s"   $occurences occurences")
      log(s"   ${srt.flatMap(_.lines).mkString("").take(2000)}...")
      log("")
    }
    log("Done")
    engine.close
  }










}

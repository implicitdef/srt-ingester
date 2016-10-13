package ingester

import java.io._
import java.net.URL
import java.nio.charset.Charset
import java.nio.file.Files

import com.github.implicitdef.toolbox.http.StandaloneWsClient
import com.github.wtekiela.opensub4j.impl.OpenSubtitlesImpl
import com.github.wtekiela.opensub4j.response.SubtitleInfo
import net.lingala.zip4j.core.ZipFile
import org.mozilla.intl.chardet.{nsDetector, nsICharsetDetectionObserver, nsPSMDetector}

import scala.collection.JavaConversions._
import scala.concurrent.Future

class Engine {

  private val login = "roliatm".reverse
  private val pass = "oRddnM98NMNV6a3".reverse
  private val userAgent = "tnegAresUtseTSO".reverse
  private val url = "http://api.opensubtitles.org/xml-rpc"
  private val httpClient = new StandaloneWsClient
  val api = new OpenSubtitlesImpl(new URL(url))

  api.login(login, pass, "en", userAgent)

  def findAllSubtitlesInfos(
     language: String,
     file: File,
     movieNames: Seq[String],
     season: String = "",
     episode: String = ""
   ): Future[Seq[SubtitleInfo]] = {
    val fromSearchByFile = Future {
      api.searchSubtitles("", file).toSeq
    }
    val fromSearchByImdbId =
      Future.traverse(movieNames) { movieName =>
        Future {
          api.searchMoviesOnImdb(movieName).toSeq.headOption.map { movieInfo =>
            api.searchSubtitles("", movieInfo.getId.toString).toSeq
          }.getOrElse {
            warn(s"$movieName not found on IMDB")
            Nil
          }
        }
      }.map(_.flatten)
    val fromSearchByQuery =
      Future.traverse(movieNames) { movieName =>
        Future {
          api.searchSubtitles("", movieName, season, episode).toSeq
        }
      }.map(_.flatten)

    for {
      seqA <- fromSearchByFile
      seqB <- fromSearchByImdbId
      seqC <- fromSearchByQuery
    } yield (
      seqA ++ seqB ++ seqC
    )
      .filter(_.getLanguage == language)
      .distinctBy(_.getId)
  }

  def downloadToFolderAndReturnsFiles(subtitleInfo: SubtitleInfo, folder: File): Future[Seq[File]] = {
    folder.mkdirs()
    val zipUrl = subtitleInfo.getZipDownloadLink
    log(s">> GET $zipUrl")
    httpClient.url(zipUrl).get().map { response =>
      if (response.isBad)
        err(s"got ${response.status} from $url")
      val bytes = response.bodyAsBytes
      // write zip
      val tmpZipFile = Files.createTempFile("srt-ingester", ".zip").toFile
      val bos = new BufferedOutputStream(new FileOutputStream(tmpZipFile))
      log(s"Unzipping to ${tmpZipFile.getAbsolutePath}")
      Stream.continually(bos.write(bytes.toArray))
      bos.close()
      // unzip in destination
      val zipFile = new ZipFile(tmpZipFile)
      zipFile.extractAll(folder.getPath)
      // remove all non .srt
      folder.listFiles().toSeq.foreach { extractedFile =>
        if (! extractedFile.getName.endsWith(".srt")) {
          log(s"Removing non .srt file ${extractedFile.getAbsolutePath}")
          extractedFile.delete()
        }
      }
      if (folder.listFiles.toSeq.isEmpty){
        log(s"Removing empty folder ${folder.getAbsolutePath}")
        folder.delete()
        Nil
      } else folder.listFiles().toSeq
    }
  }

  def readBytes(file: File): Array[Byte] = {
    Files.readAllBytes(file.toPath)
  }

  def guessCharset(bytes: Array[Byte]): Option[Charset] = {
    //TODO use UTF-8 si none ? check the results
    val detector = new nsDetector(nsPSMDetector.ALL)
    var charsetFound: Option[Charset] = None
    detector.Init(new nsICharsetDetectionObserver {
      override def Notify(charsetName: String): Unit = {
        charsetFound = Some(Charset.forName(charsetName))
      }
    })
    detector.DoIt(bytes, bytes.length, false)
    detector.DataEnd()
    charsetFound
  }


  def close() =
    httpClient.close()

}

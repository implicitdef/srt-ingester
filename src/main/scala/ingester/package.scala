import com.github.implicitdef.toolbox.Pimp
import play.api.libs.ws.WSResponse

package object ingester extends Pimp {


  case class Movie(id: Int, title: String)

  implicit val ec = scala.concurrent.ExecutionContext.Implicits.global

  def log(message: String) = println("INFO " + message)
  def warn(message: String) = println("WARN " + message)

  implicit class RichSeq[A](seq: Seq[A]){
    def distinctBy[B](func: A => B): Seq[A] =
      seq.groupBy(func).map(_._2.head).toSeq
  }

  implicit class RichWsResponse(wsResponse: WSResponse){
    def isGood: Boolean =
      200 to 299 contains wsResponse.status
    def isBad: Boolean =
      ! wsResponse.isGood
  }



}

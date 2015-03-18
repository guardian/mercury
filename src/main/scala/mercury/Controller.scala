package mercury

import javax.servlet.http.HttpServletRequest
import com.google.appengine.api.datastore.{Key, KeyFactory}
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat
import unfiltered.request._
import unfiltered.response.{ResponseString, _}

class Controller extends unfiltered.filter.Plan {
  val dateFormat = ISODateTimeFormat.dateTimeNoMillis()

  case class AvailableScan(url: String, dt: DateTime, key: Key) {
    lazy val snapshotUrl = "/snapshot?url=" + url + "&key=" + KeyFactory.keyToString(key)
    lazy val niceDate = dt.toString("EEE d MMM h:mm aa")
  }

  case class Component(name: String, promos: List[Promotion]) {
    def topLevelLinks = promos.filterNot(_.isSublink)
    def sublinksForPosition(pos: Position) = promos.filter(_.pos.idx == pos.idx).filter(_.isSublink)
  }


  def intent = {
    case GET(Path("/")) =>
      ResponseString(html.index.render().body) ~> HtmlContent

    case GET(Path("/history") & Params(p)) =>
      val url = p("url").headOption


      val history: List[(Page, List[HistoryEntry])] = url.map {
        Store.findHistory(_).groupBy(_.pos.src).toList.sortBy { case (page, _) => page.name }
      } getOrElse Nil

      ResponseString(html.history.render(url.getOrElse(""), history).body) ~> HtmlContent

    case r@ GET(Path("/history.json") & Params(p)) =>
      renderHistory(r, p, Store.findHistory)


    case r@ GET(Path("/history-by-container.json") & Params(p)) =>
      renderHistory(r, p, Store.findHistoryByContainer)

    case GET(Path("/scan") & Params(p)) =>
      val url = p("url").headOption

      val promos = url.map { u =>
        PageScanner.findPromotions(Page.fromUrl(u)).toList.sortBy(_.pos)
      } getOrElse Nil

      ResponseString(html.scan.render(url getOrElse "", promos).body) ~> HtmlContent

  }

  def renderHistory(r: HttpRequest[HttpServletRequest], params: Map[String, Seq[String]], func: (String) => List[HistoryEntry]) = {
    val url = params("url").headOption getOrElse sys.error("missing url")
    val callback = params("callback").headOption
    val tz = params("tz").headOption
    val history = func(url)

    val json = renderJsonResponse(history, tz)
    val resp = callback.map(
      c => ResponseString(s"$c($json)") ~> JsContent
    ) getOrElse (
      ResponseString(json) ~> JsonContent
      )

    val origin = r.headers("Origin").toList.headOption

    Cors.headers(origin).map { originHeader =>
      Ok ~> originHeader ~> resp
    }.getOrElse {
      Ok ~> resp
    }
  }

  def renderJsonResponse(entries: List[HistoryEntry], tz: Option[String] = None): String = {
    import spray.json.DefaultJsonProtocol._
    import spray.json._

    case class HistoryResponse(
      from: Long,
      to: Long,

      formattedFrom: String,
      formattedTo: String,

      srcPageName: String,
      srcPageUrl: String,

      component: String,
      idx: Option[Int],
      sublinkIdx: Option[Int]
    )
    implicit val historyResponseFormat = jsonFormat9(HistoryResponse)

    val responses = for (entry <- entries) yield {
      HistoryResponse(
        from = entry.from.getMillis,
        to = entry.to.getMillis,
        formattedFrom = RelativeDateTimeFormatter.print(entry.from, tz = tz),
        formattedTo = RelativeDateTimeFormatter.print(entry.to, tz = tz),
        srcPageName = entry.pos.src.name,
        srcPageUrl = entry.pos.src.url.toString,
        component = entry.pos.component,
        idx = entry.pos.idx,
        sublinkIdx = entry.pos.sublinkIdx
      )
    }

    responses.toJson.compactPrint
  }


}

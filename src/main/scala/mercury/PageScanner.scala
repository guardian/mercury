package mercury

import org.joda.time.DateTime
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import org.parboiled2.{Parser, ParserInput}
import scala.collection.JavaConverters._

case class Promotion(
  dt: DateTime,
  targetUrl: String,
  pos: Position
) {
  def pretty = pos.inWords + " => " + targetUrl

  def isSublink = pos.isSublink
}

object PageScanner {
  case class SimpleLink(href: String, componentName: Option[String] = None, isSublink: Boolean) {
    def isContent: Boolean = PathClassifier(href).IsContent.run().isSuccess
  }

  def findPromotions(page: Page): Set[Promotion] = {
    val conn = page.url.openConnection()
    conn.setRequestProperty("User-Agent", "mercury; contact graham.tackley@guardian.co.uk")

    val doc = Jsoup.parse(conn.getInputStream, "UTF-8", page.url.toString)

    val elems = doc.select("a[href^=http:]").asScala

    def findDataComponent(e: Element): Option[String] =
      e.parents().asScala
        .find(_.hasAttr("data-component"))
        .map(_.attr("data-component"))

    val links = elems.flatMap { link =>

      val component = findDataComponent(link).filterNot {
        c => c == "most-popular" || c.startsWith("popular-in-")
      }

      component.map { c =>
        val href = link.attr("abs:href").takeWhile('?' != _).takeWhile('#' != _)
        val isSublink = link.parents().asScala.exists(e => e.attr("data-link-name").contains("sublink"))
        SimpleLink(href, Some(c), isSublink)
      }.toList
    }

    val grouped = links.filter(_.isContent).groupBy(_.componentName)

    val dt = DateTime.now

    val proms = for {
      (optionalComponent, links) <- grouped
      (href, topPos, sublinkPos) <- SublinkParser.positionLinks(links)
      componentName <- optionalComponent
    } yield {
      val simpleComp = componentName.split(":").map(_.trim).filterNot(_.isEmpty).last
      val position = Position(page, simpleComp, Some(topPos), sublinkPos)
      Promotion(dt, href, position)
    }

    proms.toSet
  }
}

case class PathClassifier(input: ParserInput) extends Parser {
  import org.parboiled2.CharPredicate._

  def IsContent = rule {
    NotDate ~ Date ~ "/" ~ NotDate
  }

  def Date = rule {
    Year ~ "/" ~ Month ~ "/" ~ Day
  }

  private def Year = rule { 4 times Digit}

  private def Day = rule { 2 times Digit}

  private def Month = rule {
      "jan" | "feb" | "mar" | "apr" | "may" | "jun" |
      "jul" | "aug" | "sep" | "oct" | "nov" | "dec"
  }

  private def NotDate = rule {
    oneOrMore(!Date ~ ANY)
  }
}
package mercury

import org.joda.time.{DateTime, LocalDate}
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
    def isContent: Boolean = true//GuardianPathParser(href).contentPath.run()
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

    val links = for (link <- elems) yield {
      val href = link.attr("abs:href").takeWhile('?' != _).takeWhile('#' != _)
      val comp = findDataComponent(link)
      val isSublink = link.parents().asScala.exists(_.hasClass("sublinks"))

      SimpleLink(href, comp, isSublink)
    }

    val grouped = links.filter(_.isContent).groupBy(_.componentName)

    val dt = DateTime.now

    val proms = for {
      (optionalComponent, links) <- grouped
      (href, topPos, sublinkPos) <- SublinkParser.positionLinks(links)
      componentName <- optionalComponent
    } yield {
      val simpleComp = componentName.split(":").map(_.trim).filterNot(_.isEmpty).last
      val position = Position(page, simpleComp, topPos, sublinkPos)
      Promotion(dt, href, position)
    }

    proms.toSet
  }
}

class GuardianPathParser(val input: ParserInput) extends Parser {
  import org.parboiled2.CharPredicate._

  def isContent = rule {
    anything ~ Date ~ anything ~> ((d) => true)
  }

  def Date = rule {
    (Year ~ "/" ~ Month ~ "/" ~ Day) ~>
      ((y, m, d) => new LocalDate(y, m, d))
  }

  def Year = rule { capture(4 times Digit) ~> (y => y.toInt) }

  def Day = rule { capture(1 to 2 times Digit) ~> (d => d.toInt) }

  def Month = rule {
    Map(
      "jan" -> 1, "feb" -> 2, "mar" -> 3, "apr" -> 4, "may" -> 5, "jun" -> 6,
      "jul" -> 7, "aug" -> 8, "sep" -> 9, "oct" -> 10, "nov" -> 11, "dec" -> 12
    )
  }

  def anything = rule {
    oneOrMore(!Date ~ ANY)
  }
}
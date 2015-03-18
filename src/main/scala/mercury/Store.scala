package mercury

import java.net.URL
import java.util.Date
import com.google.appengine.api.datastore.Query.{CompositeFilterOperator, FilterOperator}
import com.google.appengine.api.datastore.{Link => GaeLink, _}
import org.joda.time.DateTime
import org.slf4j.LoggerFactory
import scala.collection.JavaConverters._

object Store {

  private val log = LoggerFactory.getLogger(getClass)
  private val ds = DatastoreServiceFactory.getDatastoreService

  implicit class UrlToGaeLink(url: URL) {
    def asLink: GaeLink = new GaeLink(url.toString)
  }

  implicit class StringToGaeLink(url: String) {
    def asLink: GaeLink = new GaeLink(url)
  }


  def write(scannedUrl: URL, promotedLinks: Set[Promotion]) {
    log.info("Writing {} links to store...", promotedLinks.size)

    val dt = promotedLinks.head.dt
    val lastScanWithin = dt.minus(Config.readingIsLatestIfWithin)

    val historyEntities = for (link <- promotedLinks) yield {
      val entity = findHistoryEntity(link, lastScanWithin) getOrElse {
        log.info("Creating new history entry for " + link.pretty)
        val e = new Entity("history")
        writePosition(link.pos, e)
        e.setProperty("from", dt.toDate)
        e.setProperty("targetUrl", link.targetUrl.asLink)
        e
      }

      entity.setProperty("to", dt.toDate)
      entity
    }

    log.info("writing updates")
    ds.put(historyEntities.asJava)
    log.info("done")
  }

  private def findHistoryEntity(promotion: Promotion, since: DateTime): Option[Entity] = {
    val filters = List(
      FilterOperator.EQUAL.of("pos", promotion.pos.inWords),
      FilterOperator.EQUAL.of("targetUrl", promotion.targetUrl.asLink),
      FilterOperator.GREATER_THAN_OR_EQUAL.of("to", since.toDate)
    )

    val q = new Query("history")
      .setFilter(CompositeFilterOperator.and(filters: _*))
      .addSort("to", Query.SortDirection.DESCENDING)

    val result = ds.prepare(q).asIterator.asScala.toList

    if (result.size > 1) {
      log.warn(s"Oh. I got ${result.size} history entries back for $promotion since $since:\n${result.mkString("\n")}")
    }

    result.headOption
  }


  private def parsePositionFromEntity(e: Entity): Position = {
    Position(
      src = Page.fromUrl(e.getProperty("srcUrl").asInstanceOf[GaeLink].getValue),
      component = e.getProperty("component").toString,
      idx = Option(e.getProperty("topPosition").asInstanceOf[Long].toInt),
      sublinkIdx = Option(e.getProperty("sublinkPosition")).map(_.asInstanceOf[Long].toInt)
    )
  }

  private def writePosition(p: Position, e: Entity) {
    e.setProperty("srcUrl", p.src.url.asLink)
    e.setProperty("component", p.component)
    p.idx.foreach(e.setProperty("topPosition", _))
    p.sublinkIdx.foreach(e.setProperty("sublinkPosition", _))
    e.setProperty("pos", p.inWords)
  }

  private def readHistoryEntry(e: Entity): HistoryEntry = {
    HistoryEntry(
      from = new DateTime(e.getProperty("from").asInstanceOf[Date]),
      to = new DateTime(e.getProperty("to").asInstanceOf[Date]),
      targetUrl = e.getProperty("targetUrl").asInstanceOf[GaeLink].getValue,
      pos = parsePositionFromEntity(e)
    )
  }

  def findHistory(url: String): List[HistoryEntry] = {
    val q = new Query("history")
      .setFilter(FilterOperator.EQUAL.of("targetUrl", url.asLink))
    ds.prepare(q).asIterable.asScala.map(readHistoryEntry).toList.sortBy(_.from).reverse
  }

  def findHistoryByContainer(url: String): List[HistoryEntry] = {
    val history = findHistory(url).filterNot {
      h => h.pos.component == "most-popular" || h.pos.component.startsWith("popular-in-")
    }

    val groupedByFront = history.groupBy(_.pos.src.url.toString)

    groupedByFront.map {
      case (frontUrl, historyEntriesOnFront) =>
        val groupedByComponent = historyEntriesOnFront.groupBy(_.pos.component)
        groupedByComponent.map {
          case (componentName, historyEntriesInComponent) =>
            val firstSeen = reduce(historyEntriesInComponent, min).from.getMillis
            val lastSeen = reduce(historyEntriesInComponent, max).to.getMillis

            val position = Position(
              src = historyEntriesInComponent.head.pos.src,
              component = componentName
            )

            HistoryEntry(
              from = new DateTime(firstSeen),
              to = new DateTime(lastSeen),
              targetUrl = url,
              pos = position
            )
        }
    }.toList.flatten.sortBy(_.from).reverse
  }

  type ReduceLeftComparator = (DateTime, DateTime) => Boolean

  def reduce(entries: List[HistoryEntry], comparator: ReduceLeftComparator): HistoryEntry =
    entries.reduceLeft((l,r) => if(comparator(l.from, r.from)) l else r)

  def min(thiz: DateTime, that: DateTime): Boolean = thiz.getMillis < that.getMillis
  def max(thiz: DateTime, that: DateTime): Boolean = thiz.getMillis > that.getMillis
}

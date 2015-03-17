package mercury

import org.joda.time.{DateTime, DateTimeZone}

object RelativeDateTimeFormatter {

  private lazy val london = DateTimeZone.forID("Europe/London")
  private lazy val newYork = DateTimeZone.forID("America/New_York")
  private lazy val sydney = DateTimeZone.forID("Australia/Sydney")

  def print(dt: DateTime, relativeTo: DateTime = DateTime.now, tz: Option[String] = None): String = {
    if (dt.isAfter(relativeTo.minus(Config.readingIsLatestIfWithin))) "now"
    else {
      val timezone = tz match {
        case Some("America/New_York") => newYork
        case Some("Australia/Sydney") => sydney
        case _ => london
      }
      dt.withZone(timezone).toString("d MMM HH:mm")
    }
  }
}

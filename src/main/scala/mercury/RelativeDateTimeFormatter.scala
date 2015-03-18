package mercury

import org.joda.time.{DateTime, DateTimeZone}

object RelativeDateTimeFormatter {

  def print(dt: DateTime, relativeTo: DateTime = DateTime.now, tz: Option[String] = None): String = {
    if (dt.isAfter(relativeTo.minus(Config.readingIsLatestIfWithin))) "now"
    else {

      val timezoneId = DateTimeZone.forID(tz.getOrElse("Europe/London"))
      dt.withZone(timezoneId).toString("d MMM HH:mm")
    }
  }
}

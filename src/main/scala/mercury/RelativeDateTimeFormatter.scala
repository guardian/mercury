package mercury

import org.joda.time.{DateTime, DateTimeZone}

object RelativeDateTimeFormatter {

  private lazy val london = DateTimeZone.forID("Europe/London")

  def print(dt: DateTime, relativeTo: DateTime = DateTime.now): String = {
    if (dt.isAfter(relativeTo.minus(Config.readingIsLatestIfWithin)))
      "now"
    else
      dt.withZone(london).toString("d MMM HH:mm")
  }


}

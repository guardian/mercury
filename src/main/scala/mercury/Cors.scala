package mercury

import unfiltered.response.ResponseHeader

object Cors {

  private val acceptedHosts = Seq(
    "ophan.co.uk",
    "localhost:"
  )

  def headers(origin: Option[String]): Option[ResponseHeader] = {

    def isWhiteListedHost(originHost: String) =
      acceptedHosts.exists(h => originHost.contains(h))

    def buildHeaders(origin: String) =
        ResponseHeader("Access-Control-Allow-Origin", Seq(origin))

    origin.filter(isWhiteListedHost).map(buildHeaders)
  }
}
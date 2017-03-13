package org.arktos

object URI {
  type URIType = Map[String, Any]

  val encoder = new URIEncoder()

  def get = Map.empty: URIType

  def build(uri: URIType): String = {
    (if (uri.contains("scheme")) (uri("scheme") + ":") else "") +
      (if (uri.contains("authority")) {
        "//" + uri("authority")
      } else {
        if (uri.contains("host")) {
          "//" +
            (if (uri.contains("userinfo")) { uri("userinfo") + "@" }
            else {
              if (uri.contains("user")) {
                uri("user") + (if (uri.contains("password")) { ":" + uri("password") } else "") + "@"
              } else ""
            }) +
            uri("host") +
            (if (uri.contains("port")) { ":" + uri("port") } else "")
        } else {
          if (uri.contains("ipv4address")) uri("ipv4address")
          else if (uri.contains("ipv6address")) { "[" + uri("ipv6address") + "]" }
          else if (uri.contains("ipv6addressz")) { "[" + uri("ipv6addressz") + "]" }
          else if (uri.contains("ipvfuture")) { "[v" + uri("ipvfuture") + "]" }
          else if (uri.contains("ipvfuturelinklocal")) { "[v1" + uri("ipvfuturelinklocal") + "]" }
          else if (uri.contains("regname")) uri("regname")
          else ""
        }
      }) +
      uri.getOrElse("path", "") +
      (if (uri.contains("params")) ("?" + (uri("params").asInstanceOf[List[(String, String)]].map({ case (k, null) ⇒ k; case (k, v) ⇒ k + "=" + encoder.encode(v) }).mkString("&"))) else "") +
      (if (uri.contains("hash")) uri("hash") else (if (uri.contains("fragment")) { "#" + uri("fragment") } else ""))
  }
}

package org.arktos

import org.arktos.URI._

trait URIBuilder {
  def builder(uri: URIType) = {
    (if (uri.contains("scheme")) (uri("scheme") + ":") else (if (uri.contains("protocol")) uri("protocol") + ":" else "")) +
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
            encoder.encode(uri("host").toString, notEncoded)
        } else {
          if (uri.contains("ipv4address")) uri("ipv4address")
          else if (uri.contains("ipv6address")) { "[" + uri("ipv6address") + "]" }
          else if (uri.contains("ipv6addressz")) { "[" + uri("ipv6addressz") + "]" }
          else if (uri.contains("ipvfuture")) { "[v" + uri("ipvfuture") + "]" }
          else if (uri.contains("ipvfuturelinklocal")) { "[v1" + uri("ipvfuturelinklocal") + "]" }
          else if (uri.contains("hostname")) {
            encoder.encode(uri("hostname").toString, notEncoded) +
              (if (uri.contains("port")) { ":" + uri("port") } else "")
          } else ""
        }
      }) +
      encoder.encode(uri.getOrElse("path", "").toString) +
      (if (uri.contains("params")) ("?" + (uri.getParamsAsList().map({ case (k, "") ⇒ k; case (k, v) ⇒ encoder.encode(k, notEncoded -- '=', true) + "=" + encoder.encode(v, notEncoded -- '=', true) }).mkString("&"))) else "") +
      (if (uri.contains("fragment")) { "#" + encoder.encode(uri("fragment").toString, notEncoded ++ ' ') } else "")
  }
}

object URIBuilder extends URIBuilder
/*
* Copyright (C) 2014-2017 Juergen Pfundt
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/

package org.arktos

import org.arktos.URI._

trait URIBuilder {
  def builder(uri: URIType): String = {
    (if (uri.contains("scheme")) (uri("scheme") + ":") else (if (uri.contains("protocol")) uri("protocol") + ":" else "")) +
      ( /*if (uri.contains("authority")) {
        "//" + uri("authority")
      } else {*/
        if (uri.contains("host")) {
          "//" +
            (if (uri.contains("userinfo")) { uri("userinfo") + "@" }
            else {
              if (uri.contains("user")) {
                userEncoder(uri("user").toString) + (if (uri.contains("password")) { ":" + userEncoder(uri("password").toString) } else "") + "@"
              } else ""
            }) +
            (if (uri.contains("hostname")) {
              hostnameEncoder(uri("hostname").toString) +
                (if (uri.contains("port")) ":" + uri("port") else "")
            } else {
              pathEncoder(uri("host").toString)
            })
        } else {
          if (uri.contains("ipv4address")) uri("ipv4address")
          else if (uri.contains("ipv6address")) { "[" + uri("ipv6address") + "]" }
          else if (uri.contains("ipv6addressz")) { "[" + uri("ipv6addressz") + "]" }
          else if (uri.contains("ipvfuture")) { "[v" + uri("ipvfuture") + "]" }
          else if (uri.contains("ipvfuturelinklocal")) { "[v1" + uri("ipvfuturelinklocal") + "]" }
          else if (uri.contains("hostname")) {
            hostnameEncoder(uri("hostname").toString) +
              (if (uri.contains("port")) { ":" + uri("port") } else "")
          } else ""
        }
      /*}*/ ) +
        pathEncoder(uri.getOrElse("path", "").toString) +
        (if (uri.contains("params")) ("?" + (uri.getParamsAsList().map({ case (k, None) ⇒ k; case (k, Some(v)) ⇒ queryEncoder(k) + "=" + queryEncoder(v) }).mkString("&"))) else "") +
        (if (uri.contains("fragment")) { "#" + fragmentEncoder(uri("fragment").toString) } else "")
  }
}

object URIBuilder extends URIBuilder
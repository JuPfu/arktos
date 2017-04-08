/*
* Copyright (C) 2015 - 2017 Juergen Pfundt
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

import scala.annotation.tailrec
import scala.collection.immutable.Map

object evalURI {
  def apply() = new evalURI()

  val uridecoder = new URIDecoder()

  val protocols = Set("aaa", "aaas", "about", "acap", "acct", "acr", "adiumxtra", "afp", "afs", "aim", "appdata", "apt",
    "attachment", "aw", "barion", "beshare", "bitcoin", "blob", "bolo", "callto", "cap", "chrome", "chrome-extension",
    "cid", "coap", "coaps", "com-eventbrite-attendee", "content", "crid", "cvs", "data", "dav", "dict", "dlna-playcontainer",
    "dlna-playsingle", "dns", "dntp", "dtn", "dvb", "ed2k", "example", "facetime", "fax", "feed", "feedready", "file",
    "filesystem", "finger", "fish", "ftp", "geo", "gg", "git", "gizmoproject", "go", "gopher", "gtalk", "h323", "ham",
    "hcp", "http", "https", "iax", "icap", "icon", "im", "imap", "info", "iotdisco", "ipn", "ipp", "ipps", "irc", "irc6",
    "ircs", "iris", "iris.beep", "iris.lwz", "iris.xpc", "iris.xpcs", "isostore", "itms", "jabber", "jar", "jms", "keyparc",
    "lastfm", "ldap", "ldaps", "magnet", "mailserver", "mailto", "maps", "market", "message", "mid", "mms", "modem", "ms-access",
    "ms-drive-to", "ms-excel", "ms-getoffice", "ms-help", "ms-infopath", "ms-media-stream-id", "ms-project", "ms-powerpoint",
    "ms-publisher", "ms-search-repair", "ms-secondary-screen-controller", "ms-secondary-screen-setup", "ms-settings",
    "ms-settings-airplanemode", "ms-settings-bluetooth", "ms-settings-camera", "ms-settings-cellular", "ms-settings-cloudstorage",
    "ms-settings-emailandaccounts", "ms-settings-language", "ms-settings-location", "ms-settings-lock", "ms-settings-nfctransactions",
    "ms-settings-notifications", "ms-settings-power", "ms-settings-privacy", "ms-settings-proximity", "ms-settings-screenrotation",
    "ms-settings-wifi", "ms-settings-workplace", "ms-spd", "ms-transit-to", "ms-visio", "ms-walk-to", "ms-word", "msnim",
    "msrp", "msrps", "mtqp", "mumble", "mupdate", "mvn", "news", "nfs", "ni", "nih", "nntp", "notes", "oid", "opaquelocktoken",
    "pack", "palm", "paparazzi", "pkcs11", "platform", "pop", "pres", "prospero", "proxy", "psyc", "query", "redis", "rediss",
    "reload", "res", "resource", "rmi", "rsync", "rtmfp", "rtmp", "rtsp", "rtsps", "rtspu", "secondlife", "service", "session",
    "sftp", "sgn", "shttp", "sieve", "sip", "sips", "skype", "smb", "sms", "smtp", "snews", "snmp", "soap.beep", "soap.beeps",
    "soldat", "spotify", "ssh", "steam", "stun", "stuns", "submit", "svn", "tag", "teamspeak", "tel", "teliaeid", "telnet",
    "tftp", "things", "thismessage", "tip", "tn3270", "turn", "turns", "tv", "udp", "unreal", "urn", "ut2004", "vemmi",
    "ventrilo", "videotex", "view-source", "wais", "webcal", "ws", "wss", "wtai", "wyciwyg", "xcon", "xcon-userid", "xfire",
    "xmlrpc.beep", "xmlrpc.beeps", "xmpp", "xri", "ymsgr", "z39.50", "z39.50r", "z39.50s")
}

class evalURI {

  import evalURI.{ protocols, uridecoder }
  import URIReturnValue._
  import URIAST._

  def eval(expr: URIAST): URIReturnValue = {
    (expr: @unchecked) match {
      case URI_URI(scheme, hier_part, query, fragment) ⇒
        ((((
          eval(scheme),
          eval(hier_part),
          query match {
            case Some(q) ⇒ eval(q)
            case None    ⇒ URIString("")
          },
          fragment match {
            case Some(f) ⇒ eval(f)
            case None    ⇒ URIString("")
          }
        ): @unchecked) match {
          case (URIMap(m1), URIMap(m2), URIMap(m3), URIMap(m4))       ⇒ URIMap(m1 ++ m2 ++ m3 ++ m4)
          case (URIMap(m1), URIMap(m2), URIMap(m3), URIString(s))     ⇒ URIMap(m1 ++ m2 ++ m3)
          case (URIMap(m1), URIMap(m2), URIString(s), URIMap(m4))     ⇒ URIMap(m1 ++ m2 ++ m4)
          case (URIMap(m1), URIMap(m2), URIString(s3), URIString(s4)) ⇒ URIMap(m1 ++ m2)
        }): @unchecked) match {
          case URIMap(m) ⇒ URIMap(m ++ Map("uri_type" → "absolute"))
        }
      case URI_Relative_Ref(relative_part, query, fragment) ⇒
        ((((
          eval(relative_part),
          query match {
            case Some(q) ⇒ eval(q)
            case None    ⇒ URIString("")
          },
          fragment match {
            case Some(f) ⇒ eval(f)
            case None    ⇒ URIString("")
          }
        ): @unchecked) match {
          case (URIMap(m1), URIMap(m2), URIMap(m3))       ⇒ URIMap(m1 ++ m2 ++ m3)
          case (URIMap(m1), URIMap(m2), URIString(s3))    ⇒ URIMap(m1 ++ m2)
          case (URIMap(m1), URIString(s2), URIMap(m3))    ⇒ URIMap(m1 ++ m3)
          case (URIMap(m1), URIString(s2), URIString(s3)) ⇒ URIMap(m1)
        }): @unchecked) match {
          case URIMap(m) ⇒ URIMap(m ++ Map("uri_type" → "relative"))
        }
      case URI_Relative_Part(authority, path) ⇒
        ((eval(authority), eval(path)): @unchecked) match { case (URIMap(m1), URIMap(m2)) ⇒ URIMap(m1 ++ m2) }
      case URI_Relative_Part_Path(path) ⇒ eval(path)
      case URI_Scheme(s)                ⇒ URIMap(Map("scheme" → s) ++ Map("protocol" → protocols.find(_ == s.toLowerCase).getOrElse("")))
      case URI_Hier_Part_Absolute(authority, path) ⇒
        ((
          eval(authority),
          eval(path)
        ): @unchecked) match {
          case (URIMap(m1), URIMap(m2)) ⇒ URIMap(Map("scheme_postfix" → "//") ++ m1 ++ m2)
        }
      case URI_Hier_Part_Path(path) ⇒ eval(path)
      case URI_Reference(rule)      ⇒ eval(rule)
      case URI_AbsoluteURI(scheme, hier_part, query) ⇒
        ((((
          eval(scheme),
          eval(hier_part),
          query match {
            case Some(q) ⇒ eval(q)
            case None    ⇒ URIString("")
          }
        ): @unchecked) match {
          case (URIMap(m1), URIMap(m2), URIMap(m3))       ⇒ URIMap(m1 ++ m2 ++ m3)
          case (URIMap(m1), URIMap(m2), URIString(s))     ⇒ URIMap(m1 ++ m2)
          case (URIMap(m1), URIString(s2), URIString(s3)) ⇒ URIMap(m1)
        }): @unchecked) match {
          case URIMap(m) ⇒ URIMap(m ++ Map("uri_type" → "absolute"))
        }
      case URI_Authority(userinfo, host, port) ⇒
        ((userinfo match {
          case Some(u) ⇒ eval(u)
          case None    ⇒ URIString("")
        }, eval(host), port match {
          case Some(p) ⇒ eval(p)
          case None    ⇒ URIString("")
        }): @unchecked) match {
          case (URIMap(m1), URIMap(m2), URIMap(m3)) ⇒ URIMap(m1 ++ m2 ++ m3 ++
            Map("authority" → (m1("userinfo") + "@" + m2("hostname") + ":" + m3("port"))) ++
            Map("host" → (m2("hostname") + ":" + m3("port"))))
          case (URIMap(m1), URIMap(m2), URIString(s)) ⇒ URIMap(m1 ++ m2 ++
            Map("authority" → ((m1("userinfo") + "@" + m2("hostname")))) ++
            Map("host" → (m2("hostname"))))
          case (URIString(s), URIMap(m2), URIMap(m3)) ⇒ URIMap(m2 ++ m3 ++
            Map("authority" → (m2("hostname") + ":" + m3("port"))) ++
            Map("host" → (m2("hostname") + ":" + m3("port"))))
          case (URIString(s1), URIMap(m2), URIString(s3)) ⇒ URIMap(m2 ++ Map("authority" → m2("hostname")) ++ Map("host" → m2("hostname")))
        }
      case URI_UserInfo(user, password) ⇒
        ((eval(user), password match {
          case Some(p) ⇒ eval(p)
          case None    ⇒ URIString("")
        }): @unchecked) match {
          case (URIMap(m1), URIMap(m2))   ⇒ URIMap(m1 ++ m2 ++ Map("userinfo" → (m1("user") + ":" + m2("password"))))
          case (URIMap(m1), URIString(s)) ⇒ URIMap(m1 ++ Map("userinfo" → m1("user")))
        }
      case URI_User(user)         ⇒ URIMap(Map("user" → uridecoder.decode(user)))
      case URI_Password(password) ⇒ URIMap(Map("password" → uridecoder.decode(password)))
      case URI_Reg_Name(name)     ⇒ URIString(uridecoder.decode(name))
      case URI_Port(port)         ⇒ URIMap(Map("port" → port))
      case URI_Path(path)         ⇒ eval(path)
      case URI_Path_AbEmpty(path_abempty) ⇒
        val p = uridecoder.decode(path_abempty.foldLeft("")((x, y) ⇒ x + "/" + y))
        val f = new java.io.File(p)
        val i = f.getName.lastIndexOf(".")
        val suffix = if (i >= 0 && i < f.getName.length - 1) f.getName.substring(i + 1) else ""
        val d = f.getParent
        URIMap(Map("path" → p) ++ Map("filename" → f.getName) ++ (if (i >= 0) Map("suffix" → suffix) else Map.empty: URIType) ++
          (if (d != null) Map("directory" → f.getParent) else Map.empty: URIType) ++
          Map("segment" → path_abempty.foldLeft(List.empty[String])((x, y) ⇒ x :+ uridecoder.decode(y))))
      case URI_Path_Absolute(path_absolute) ⇒
        val p = uridecoder.decode(path_absolute.foldLeft("")((x, y) ⇒ x + "/" + y))
        val f = new java.io.File(p)
        val i = f.getName.lastIndexOf(".")
        val suffix = if (i >= 0 && i < f.getName.length - 1) f.getName.substring(i + 1) else ""
        val d = f.getParent
        URIMap(Map("path" → p) ++ Map("filename" → f.getName) ++ (if (i >= 0) Map("suffix" → suffix) else Map.empty: URIType) ++
          (if (d != null) Map("directory" → f.getParent) else Map.empty: URIType) ++
          Map("segment" → path_absolute.foldLeft(List.empty[String])((x, y) ⇒ x :+ uridecoder.decode(y))))
      case URI_Path_NoScheme(path_noscheme) ⇒
        val p = uridecoder.decode(path_noscheme.mkString("/"))
        val f = new java.io.File(p)
        val i = f.getName.lastIndexOf(".")
        val suffix = if (i >= 0 && i < f.getName.length - 1) f.getName.substring(i + 1) else ""
        val d = f.getParent
        URIMap(Map("path" → p) ++ Map("filename" → f.getName) ++ (if (i >= 0) Map("suffix" → suffix) else Map.empty: URIType) ++
          (if (d != null) Map("directory" → f.getParent) else Map.empty: URIType) ++
          Map("segment" → path_noscheme.foldLeft(List.empty[String])((x, y) ⇒ x :+ uridecoder.decode(y))))
      case URI_Path_Rootless(path_rootless) ⇒
        val p = uridecoder.decode(path_rootless.mkString("/"))
        val f = new java.io.File(p)
        val i = f.getName.lastIndexOf(".")
        val suffix = if (i >= 0 && i < f.getName.length - 1) f.getName.substring(i + 1) else ""
        val d = f.getParent
        URIMap(Map("path" → p) ++ Map("filename" → f.getName) ++ (if (i >= 0) Map("suffix" → suffix) else Map.empty: URIType) ++
          (if (d != null) Map("directory" → f.getParent) else Map.empty: URIType) ++
          Map("segment" → path_rootless.foldLeft(List.empty[String])((x, y) ⇒ x :+ uridecoder.decode(y))))
      case URI_Path_Empty(path_empty) ⇒ URIMap(Map("path" → path_empty))
      case URI_Host(rule) ⇒
        val hostname = eval(rule)
        (rule: @unchecked) match {
          case URI_IP_Literal(literal)      ⇒ (hostname: @unchecked) match { case URIMap(m) ⇒ URIMap(m ++ Map("hostname" → m("ipliteral"))) }
          case URI_IPv4Address(ipv4address) ⇒ (hostname: @unchecked) match { case URIString(s) ⇒ URIMap(Map("hostname" → s) ++ Map("ipv4address" → s)) }
          case URI_Reg_Name(regname) ⇒ (hostname: @unchecked) match {
            case URIString(s) ⇒ URIMap(Map("hostname" → s) ++
              Map("domain" → s.split('.').toList.tail.mkString(".")) ++
              Map("subdomain" → s.split('.').toList.head) ++
              Map("tld" → s.split('.').toList.last))
          }
        }
      case URI_IP_Literal(ipLiteral) ⇒
        val literal = eval(ipLiteral)
        (ipLiteral: @unchecked) match {
          case URI_IPv6Address(ip)                   ⇒ (literal: @unchecked) match { case URIMap(m) ⇒ URIMap(m ++ Map("ipliteral" → ("[" + m("ipv6address") + "]"))) }
          case URI_IPv6AddressZ(address, zone)       ⇒ (literal: @unchecked) match { case URIMap(m) ⇒ URIMap(m ++ Map("ipliteral" → ("[" + m("ipv6addressz") + "]"))) }
          case URI_IPvFuture(ip)                     ⇒ (literal: @unchecked) match { case URIMap(m) ⇒ URIMap(m ++ Map("ipliteral" → ("[v" + m("ipvfuture") + "]"))) }
          case URI_IPvFutureLinkLocal(address, zone) ⇒ (literal: @unchecked) match { case URIMap(m) ⇒ URIMap(m ++ Map("ipliteral" → ("[v1." + m("ipvfuturelinklocal") + "]"))) }
        }
      case URI_IPvFuture(ipvfuture) ⇒ URIMap(Map("ipvfuture" → ipvfuture))
      case URI_IPvFutureLinkLocal(ipv6Address, zoneID) ⇒ ((eval(ipv6Address), eval(zoneID)): @unchecked) match {
        case (URIMap(m1), URIString(z)) ⇒ URIMap(Map("ipvfuturelinklocal" → (m1("ipv6Address").toString + "%" + z)) ++ Map("zoneid" → z))
      }
      case URI_ZoneID(zoneID)       ⇒ URIString(uridecoder.decode(zoneID))
      case URI_IPv6Address(address) ⇒ URIMap(Map("ipv6address" → address))
      case URI_IPv6AddressZ(address, zone) ⇒
        val ipv6Address = ((eval(address): @unchecked) match { case URIMap(m) ⇒ m("ipv6address") })
        val zoneid = (eval(zone): @unchecked) match { case URIString(z) ⇒ z }
        URIMap(Map("ipv6addressz" → (ipv6Address + "%" + zoneid)) ++ Map("zoneid" → zoneid))
      case URI_IPv4Address(address) ⇒ URIString(address)
      case URI_Query(rule) ⇒
        val params = traverseParameterList(rule, Nil)
        val decoded_params = params.map((x) ⇒ (uridecoder.decode(x._1), if (x._2 != null) uridecoder.decode(x._2) else x._2))
        URIMap(
          Map("params" → decoded_params) ++
            Map("raw_params" → params) ++
            Map("query" → (params.map((x) ⇒ uridecoder.decode(x._1) + (if (x._2 != null) "=" + uridecoder.decode(x._2) else "")).mkString("&"))) ++
            Map("raw_query" → (params.map((x) ⇒ x._1 + (if (x._2 != null) "=" + x._2 else "")).mkString("&")))
        )
      case URI_QueryParameter(queryVariable, queryValue) ⇒
        ((eval(queryVariable), eval(queryValue)): @unchecked) match {
          case (URIString(variable), URIString(value)) ⇒ URITuple((variable, value))
        }
      case URI_QueryVariable(queryVariable) ⇒ URIString(queryVariable)
      case URI_QueryValue(queryValue)       ⇒ URIString(queryValue)
      case URI_QueryToken(queryToken)       ⇒ URIString(queryToken)
      case URI_Fragment(fragment) ⇒
        val decoded_fragment = uridecoder.decode(fragment)
        URIMap(
          Map("fragment" → decoded_fragment) ++
            Map("hash" → ("#" + decoded_fragment)) ++
            Map("raw_fragment" → fragment)
        )
      case Error(e) ⇒ URIString("Error" + e)
    }
  }

  @tailrec
  private def traverseParameterList(l: Seq[URIAST], params: List[(String, String)]): List[(String, String)] = l match {
    case x +: xs ⇒ (eval(x): @unchecked) match {
      case URITuple((k, v)) ⇒ traverseParameterList(xs, params ::: (k, v) :: Nil)
      case URIString(t)     ⇒ traverseParameterList(xs, params ::: (t, "") :: Nil)
    }
    case Nil ⇒ params
  }
}

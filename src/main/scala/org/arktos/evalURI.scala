/*
* Copyright (C) 2015 Juergen Pfundt
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

import org.arktos.URIParser._

import scala.annotation.tailrec

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

  def eval(expr: URI_AST): URI_Return_Value = {
    expr match {
      case URI_URI(scheme, hier_part, query, fragment) ⇒
        ((((eval(scheme),
          eval(hier_part),
          query match {
            case Some(q) ⇒ eval(q)
            case None    ⇒ URI_String("")
          },
          fragment match {
            case Some(f) ⇒ eval(f)
            case None    ⇒ URI_String("")
          }): @unchecked) match {
          case (URI_Map(m1), URI_Map(m2), URI_Map(m3), URI_Map(m4))       ⇒ URI_Map(m1 ++ m2 ++ m3 ++ m4)
          case (URI_Map(m1), URI_Map(m2), URI_Map(m3), URI_String(s))     ⇒ URI_Map(m1 ++ m2 ++ m3)
          case (URI_Map(m1), URI_Map(m2), URI_String(s), URI_Map(m4))     ⇒ URI_Map(m1 ++ m2 ++ m4)
          case (URI_Map(m1), URI_Map(m2), URI_String(s3), URI_String(s4)) ⇒ URI_Map(m1 ++ m2)
        }): @unchecked) match {
          case URI_Map(m) ⇒ URI_Map(m ++ Map("uri_type" -> Left("absolute")))
        }
      case URI_Relative_Ref(relative_part, query, fragment) ⇒
        ((((eval(relative_part),
          query match {
            case Some(q) ⇒ eval(q)
            case None    ⇒ URI_String("")
          },
          fragment match {
            case Some(f) ⇒ eval(f)
            case None    ⇒ URI_String("")
          }): @unchecked) match {
          case (URI_Map(m1), URI_Map(m2), URI_Map(m3))       ⇒ URI_Map(m1 ++ m2 ++ m3)
          case (URI_Map(m1), URI_Map(m2), URI_String(s3))    ⇒ URI_Map(m1 ++ m2)
          case (URI_Map(m1), URI_String(s2), URI_Map(m3))    ⇒ URI_Map(m1 ++ m3)
          case (URI_Map(m1), URI_String(s2), URI_String(s3)) ⇒ URI_Map(m1)
        }): @unchecked) match {
          case URI_Map(m) ⇒ URI_Map(m ++ Map("uri_type" -> Left("relative")))
        }
      case URI_Relative_Part(authority, path) ⇒
        ((eval(authority), eval(path)): @unchecked) match {
          case (URI_Map(m1), URI_Map(m2)) ⇒ URI_Map(m1 ++ m2)
        }
      case URI_Relative_Part_Path(path) ⇒ eval(path)
      case URI_Scheme(s)                ⇒ URI_Map(Map("scheme" -> Left(s)) ++ Map("protocol" -> Left(protocols.find((p: String) ⇒ p == s.toLowerCase).getOrElse(""))))
      case URI_Hier_Part_Absolute(authority, path) ⇒
        ((eval(authority),
          eval(path)): @unchecked) match {
          case (URI_Map(m1), URI_Map(m2)) ⇒ URI_Map(Map("scheme_postfix" -> Left("//")) ++ m1 ++ m2)
        }
      case URI_Hier_Part_Path(path) ⇒ eval(path)
      case URI_Reference(rule)      ⇒ eval(rule)
      case URI_AbsoluteURI(scheme, hier_part, query) ⇒
        ((((eval(scheme),
          eval(hier_part),
          query match {
            case Some(q) ⇒ eval(q)
            case None    ⇒ URI_String("")
          }): @unchecked) match {
          case (URI_Map(m1), URI_Map(m2), URI_Map(m3))       ⇒ URI_Map(m1 ++ m2 ++ m3)
          case (URI_Map(m1), URI_Map(m2), URI_String(s))     ⇒ URI_Map(m1 ++ m2)
          case (URI_Map(m1), URI_String(s2), URI_String(s3)) ⇒ URI_Map(m1)
        }): @unchecked) match {
          case URI_Map(m) ⇒ URI_Map(m ++ Map("uri_type" -> Left("absolute")))
        }
      case URI_Authority(userinfo, host, port) ⇒
        ((userinfo match {
          case Some(u) ⇒ eval(u)
          case None    ⇒ URI_String("")
        }, eval(host), port match {
          case Some(p) ⇒ eval(p)
          case None    ⇒ URI_String("")
        }): @unchecked) match {
          case (URI_Map(m1), URI_Map(m2), URI_Map(m3)) ⇒ URI_Map(m1 ++ m2 ++ m3 ++
            Map("authority" -> m1("userinfo").left.map(_ + "@" + m2("hostname").left.get + ":" + m3("port").left.get)) ++
            Map("host" -> m2("hostname").left.map(_ + ":" + m3("port").left.get)))
          case (URI_Map(m1), URI_Map(m2), URI_String(s)) ⇒ URI_Map(m1 ++ m2 ++
            Map("authority" -> m1("userinfo").left.map(_ + "@" + m2("hostname").left.get)) ++
            Map("host" -> m2("hostname")))
          case (URI_String(s), URI_Map(m2), URI_Map(m3)) ⇒ URI_Map(m2 ++ m3 ++
            Map("authority" -> m2("hostname").left.map(_ + ":" + m3("port").left.get)) ++
            Map("host" -> m2("hostname").left.map(_ + ":" + m3("port").left.get)))
          case (URI_String(s1), URI_Map(m2), URI_String(s3)) ⇒ URI_Map(m2 ++ Map("authority" -> m2("hostname")) ++ Map("host" -> m2("hostname")))
        }
      case URI_UserInfo(user, password) ⇒
        ((eval(user), password match {
          case Some(p) ⇒ eval(p)
          case None    ⇒ URI_String("")
        }): @unchecked) match {
          case (URI_Map(m1), URI_Map(m2))   ⇒ URI_Map(m1 ++ m2 ++ Map("userinfo" -> m1("user").left.map(_ + ":" + m2("password").left.get)))
          case (URI_Map(m1), URI_String(s)) ⇒ URI_Map(m1 ++ Map("userinfo" -> m1("user")))
        }
      case URI_User(user)                   ⇒ URI_Map(Map("user" -> Left(uridecoder.decode(user))))
      case URI_Password(password)           ⇒ URI_Map(Map("password" -> Left(uridecoder.decode(password))))
      case URI_Reg_Name(name)               ⇒ URI_String(uridecoder.decode(name))
      case URI_Port(port)                   ⇒ URI_Map(Map("port" -> Left(port)))
      case URI_Path(path)                   ⇒ eval(path)
      case URI_Path_AbEmpty(path_abempty)   ⇒ URI_Map(Map("path" -> Left(uridecoder.decode(path_abempty.foldLeft("")((x, y) ⇒ x + "/" + y)))))
      case URI_Path_Absolute(path_absolute) ⇒ URI_Map(Map("path" -> Left(uridecoder.decode(path_absolute.foldLeft("")((x, y) ⇒ x + "/" + y)))))
      case URI_Path_NoScheme(path_noscheme) ⇒ URI_Map(Map("path" -> Left(uridecoder.decode(path_noscheme.mkString("/")))))
      case URI_Path_Rootless(path_rootless) ⇒ URI_Map(Map("path" -> Left(uridecoder.decode(path_rootless.mkString("/")))))
      case URI_Path_Empty(path_empty)       ⇒ URI_Map(Map("path" -> Left(path_empty)))
      case URI_Host(rule) ⇒ (eval(rule): @unchecked) match {
        case URI_String(s) ⇒ URI_Map(Map("hostname" -> Left(uridecoder.decode(s))))
      }
      case URI_IP_Literal(rule)     ⇒ eval(rule)
      case URI_IPvFuture(ipvfuture) ⇒ URI_String("[" + ipvfuture + "]")
      case URI_IPv6Address(address) ⇒ URI_String("[" + address + "]")
      case URI_IPv4Address(address) ⇒ URI_String(address)
      case URI_Query(rule) ⇒
        val params = traverseParameterList(rule, Nil)
        val decoded_params = params.map((x) ⇒ (uridecoder.decode(x._1), if (x._2 != null) uridecoder.decode(x._2) else x._2))
        URI_Map(
          Map("params" -> Right(decoded_params)) ++
            Map("raw_params" -> Right(params)) ++
            Map("query" -> Left(params.map((x) ⇒ uridecoder.decode(x._1) + (if (x._2 != null) "=" + uridecoder.decode(x._2) else "")).mkString("&"))) ++
            Map("raw_query" -> Left(params.map((x) ⇒ x._1 + (if (x._2 != null) "=" + x._2 else "")).mkString("&")))
        )
      case URI_QueryParameter(queryVariable, queryValue) ⇒
        ((eval(queryVariable), eval(queryValue)): @unchecked) match {
          case (URI_String(variable), URI_String(value)) ⇒ URI_Tuple((variable, value))
        }
      case URI_QueryVariable(queryVariable) ⇒ URI_String(queryVariable)
      case URI_QueryValue(queryValue)       ⇒ URI_String(queryValue)
      case URI_QueryToken(queryToken)       ⇒ URI_String(queryToken)
      case URI_Fragment(fragment) ⇒
        val decoded_fragment = uridecoder.decode(fragment)
        URI_Map(
          Map("fragment" -> Left(decoded_fragment)) ++
            Map("hash" -> Left("#" + decoded_fragment)) ++
            Map("raw_fragment" -> Left(fragment))
        )
      case Error(e) ⇒ URI_String("Error" + e)
    }
  }

  @tailrec
  private def traverseParameterList(l: Seq[URI_AST], params: List[(String, String)]): List[(String, String)] = l match {
    case x +: xs ⇒ (eval(x): @unchecked) match {
      case URI_Tuple((k, v)) ⇒ traverseParameterList(xs, params ::: (k, v) :: Nil)
      case URI_String(t)     ⇒ traverseParameterList(xs, params ::: (t, null) :: Nil)
    }
    case Nil ⇒ params
  }
}

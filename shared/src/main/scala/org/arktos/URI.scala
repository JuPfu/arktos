package org.arktos

import org.arktos.URI.URIType

object URI {
  type URIType = Map[String, Any]
  type ParamsListType = List[(String, String)]
  type ParamsMapType = Map[String, List[String]]

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

  //implicit def convertString(m: Option[String]): String = m match { case Some(s) ⇒ s; case _ ⇒ "" }

  implicit class MapExtender(val m: URIType) extends AnyVal {

    def getParamsAsList(default: ParamsListType = List.empty) = {
      m.getOrElse("params", default).asInstanceOf[List[(String, String)]]
    }

    def getParamsAsMap(default: Map[String, List[String]] = Map.empty) = {
      if (m.contains("params")) m("params").asInstanceOf[List[(String, String)]].groupBy(_._1).collect { case (x, ys) ⇒ (x, ys.foldLeft(List.empty: List[String])((k, v) ⇒ k :+ v._2)) }
      else default
    }

    def setParams(p: Map[String, List[String]]) = {
      val r: List[(String, String)] = List()
      def kFactor(k: String)(l: List[String], r: List[(String, String)]): List[(String, String)] = l match {
        case x :: xs ⇒ kFactor(k)(xs, r :+ ((k, x)))
        case default ⇒ r
      }

      m.updated("params", p.foldLeft(List.empty: List[(String, String)])((x, y) ⇒ x ::: kFactor(y._1)(y._2, List())))
    }
  }

  implicit class ParamsListExtender(val l: ParamsListType) extends AnyVal {
    def toParamsMap = {
      l.groupBy(_._1).collect { case (x, ys) ⇒ (x, ys.foldLeft(List.empty: List[String])((k, v) ⇒ k :+ v._2)) }
    }
  }

  implicit class ParamsMapExtender(val m: ParamsMapType) extends AnyVal {
    def toParamsList = {
      val r: List[(String, String)] = List()
      def kFactor(k: String)(l: List[String], r: List[(String, String)]): List[(String, String)] = l match {
        case x :: xs ⇒ kFactor(k)(xs, r :+ ((k, x)))
        case default ⇒ r
      }
      m.foldLeft(List.empty: List[(String, String)])((x, y) ⇒ x ::: kFactor(y._1)(y._2, List()))
    }
  }
}


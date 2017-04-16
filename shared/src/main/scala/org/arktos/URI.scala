package org.arktos

import scala.annotation.tailrec

object URI {
  type URIType = Map[String, Any]
  type ParamsListType = List[(String, String)]
  type ParamsMapType = Map[String, List[String]]

  val encoder = new URIEncoder()

  def get = Map.empty: URIType
  def get(u: String) = {
    val parsedURI = URIParser(u)
    if (parsedURI.isSuccess) parsedURI.get else Map.empty: URIType
  }

  def build(uri: URIType): String = {
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
      @tailrec
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
      @tailrec
      def kFactor(k: String)(l: List[String], r: List[(String, String)]): List[(String, String)] = l match {
        case x :: xs ⇒ kFactor(k)(xs, r :+ ((k, x)))
        case default ⇒ r
      }
      m.foldLeft(List.empty: List[(String, String)])((x, y) ⇒ x ::: kFactor(y._1)(y._2, List()))
    }
  }
}


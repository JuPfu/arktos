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

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object URI {
  type URIType = Map[String, Any]
  type ParamsListType = List[(String, Option[String])]
  type ParamsMapType = Map[String, List[Option[String]]]

  val encoder = new URIEncoder()
  val userEncoder = encoder.encode(notEncoded, false, "UTF-8") _
  val hostnameEncoder = encoder.encode(notEncoded, false, "UTF-8") _
  val pathEncoder = encoder.encode(notEncoded, false, "UTF-8") _
  val queryEncoder = encoder.encode(notEncoded -- ' ', true, "UTF-8") _
  val fragmentEncoder = encoder.encode(notEncoded, false, "UTF-8") _

  def get = Map.empty: URIType

  def get(u: String) = {
    val parsedURI = URIParser(u)
    if (parsedURI.isSuccess) parsedURI.get else Map.empty: URIType
  }

  def build(uri: URIType, builder: URIType ⇒ String = URIBuilder.builder): String = {
    builder(uri)
  }

  def cmpOption(l: (String, Option[String]), r: (String, Option[String]))(implicit ord: Ordering[(String, Option[String])]) = {
    import ord._
    l < r
  }

  implicit class MapExtender(val m: URIType) extends AnyVal {
    def getParamsAsList(default: ParamsListType = List.empty: ParamsListType): ParamsListType = {
      m.getOrElse("params", default).asInstanceOf[ParamsListType]
    }

    def getParamsAsMap(default: ParamsMapType = Map.empty) = {
      if (m.contains("params")) m("params").asInstanceOf[ParamsListType].groupBy(_._1).collect { case (x, ys) ⇒ (x, ys.foldLeft(List.empty: List[Option[String]])((k, v) ⇒ k :+ v._2)) }
      else default
    }

    def setParams(p: Map[String, List[Option[String]]]) = {
      @tailrec
      def kFactor(k: String)(l: List[Option[String]], r: ListBuffer[(String, Option[String])]): ListBuffer[(String, Option[String])] = l match {
        case x :: xs ⇒ kFactor(k)(xs, r :+ ((k, x)))
        case default ⇒ r
      }

      m.updated("params", p.foldLeft(ListBuffer.empty: ListBuffer[(String, Option[String])])((x, y) ⇒ x ++ kFactor(y._1)(y._2, ListBuffer()))).toList
    }
  }

  implicit class ParamsListExtender(val l: ParamsListType) extends AnyVal {
    def toParamsMap: ParamsMapType = {
      l.groupBy(_._1).collect {
        case (x, ys) ⇒ (x, ys.foldLeft(List.empty: List[Option[String]])((k, v) ⇒ k :+ v._2))
      }
    }
  }

  implicit class ParamsMapExtender(val m: ParamsMapType) extends AnyVal {
    def toParamsList: ParamsListType = {
      @tailrec
      def kFactor(k: String)(l: List[Option[String]], r: ListBuffer[(String, Option[String])]): ListBuffer[(String, Option[String])] = l match {
        case x :: xs ⇒ kFactor(k)(xs, r :+ ((k, x)))
        case default ⇒ r
      }
      m.foldLeft(ListBuffer.empty: ListBuffer[(String, Option[String])])((x, y) ⇒ x ++ kFactor(y._1)(y._2, ListBuffer())).toList
    }
  }

  val defaultPortMap = Map(
    "ftp" → "20",
    "ssh" → "22",
    "telnet" → "23",
    "smtp" → "25",
    "dns" → "53",
    "tftp" → "69",
    "gopher" → "70",
    "finger" → "79",
    "http" → "80",
    "sftp" → "115",
    "imap" → "143",
    "snmp" → "161",
    "irc" → "194",
    "ldap" → "389",
    "https" → "443",
    "rtsp" → "554",
    "nntp" → "563",
    "ipp" → "631",
    "ldaps" → "636",
    "iris" → "702",
    "ircs" → "994"
  )
}


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
  type ParamsListType = List[(String, String)]
  type ParamsMapType = Map[String, List[String]]

  val encoder = new URIEncoder()

  def get = Map.empty: URIType

  def get(u: String) = {
    val parsedURI = URIParser(u)
    if (parsedURI.isSuccess) parsedURI.get else Map.empty: URIType
  }

  def build(uri: URIType, builder: URIType ⇒ String = URIBuilder.builder): String = {
    builder(uri)
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
      @tailrec
      def kFactor(k: String)(l: List[String], r: ListBuffer[(String, String)]): ListBuffer[(String, String)] = l match {
        case x :: xs ⇒ kFactor(k)(xs, r :+ ((k, x)))
        case default ⇒ r
      }

      m.updated("params", p.foldLeft(ListBuffer.empty: ListBuffer[(String, String)])((x, y) ⇒ x ++ kFactor(y._1)(y._2, ListBuffer()))).toList
    }
  }

  implicit class ParamsListExtender(val l: ParamsListType) extends AnyVal {
    def toParamsMap = {
      l.groupBy(_._1).collect { case (x, ys) ⇒ (x, ys.foldLeft(List.empty: List[String])((k, v) ⇒ k :+ v._2)) }
    }
  }

  implicit class ParamsMapExtender(val m: ParamsMapType) extends AnyVal {
    def toParamsList = {
      @tailrec
      def kFactor(k: String)(l: List[String], r: ListBuffer[(String, String)]): ListBuffer[(String, String)] = l match {
        case x :: xs ⇒ kFactor(k)(xs, r :+ ((k, x)))
        case default ⇒ r
      }
      m.foldLeft(ListBuffer.empty: ListBuffer[(String, String)])((x, y) ⇒ x ++ kFactor(y._1)(y._2, ListBuffer())).toList
    }
  }
}


package org.arktos

/**
  * Created by jp on 02.03.17.
  */

sealed trait URIReturnValue {

  case class URIString(s: String) extends URIReturnValue

  case class URITuple(s: (String, String)) extends URIReturnValue

  case class URIParam(s: List[(String, String)]) extends URIReturnValue

  //case class URI_Map(m: Map[String, Either[String, List[(String, String)]]]) extends URI_Return_Value
  case class URIMap[V >: Serializable](m: Map[String, V]) extends URIReturnValue

}

object URIReturnValue extends URIReturnValue
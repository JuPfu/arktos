package org.arktos

object URI {
  type URIType = Map[String, Serializable]

  def get = URIReturnValue.URIMap(Map.empty) match { case URIReturnValue.URIMap(x) ⇒ x }
  def build(uri: Map[String, java.io.Serializable]): String = {
    (if (uri.contains("scheme")) ( uri("scheme") + ":") else "") +
      uri.getOrElse("scheme_postfix", "") +
      uri.getOrElse("authority", "") +
      uri.getOrElse("path", "") +
      (if ( uri.contains("params")) ("?" + (uri("params").asInstanceOf[List[(String, String)]].map({ case (k, null) ⇒ k; case (k, v) ⇒ k + "=" + v }).mkString("&"))) else "") +
      uri.getOrElse("hash", "")
  }
}

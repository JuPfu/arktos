/*
* Copyright (C) 2014 Juergen Pfundt
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


import org.arktos.URIParser.URI_Map
import org.parboiled2.{ErrorFormatter, ParseError, ParserInput}
import org.scalatest.FlatSpec
import scala.util.{Success, Failure}


class arktosSpec extends FlatSpec {

  """The URI  'http://jp:secret@www.ietf.org/rfc/rfc2396.txt?p=1&p=URI#content""" must "succeed" taggedAs (rfc3986) in {

    val ms: Double = System.currentTimeMillis

    val input_uri = """http://jp:secret@www.ietf.org/rfc/rfc2396.txt?p=1&p=URI#content"""

    val parser = URIParser(input_uri)

    val res = parser.URI_reference.run() match {
      case Success(x) ⇒
        val m = ((new evalURI).eval(x): @unchecked) match {
          case URI_Map(x) ⇒ x
        }
        val uri = m.mapValues { case Left(v) ⇒ v; case (Right(v)) ⇒ v }
        val me: Double = System.currentTimeMillis - ms
        System.err.println("Used time " + (me / 1000.0))
        val p = uri.getOrElse("params", List()).asInstanceOf[List[(String,String)]].map({case (k,v) => k+"="+v}).mkString("&")
        val uri_synthesized = uri.getOrElse("protocol", uri.get("scheme")) +
          "://" + uri.getOrElse("authority", "") +
          uri.getOrElse("path", "") +
          (if ( p.length > 0 ) "?" else "") + p +
          uri.getOrElse("hash", "")
        assert(input_uri == uri_synthesized)
      case Failure(e: ParseError) ⇒ System.err.println("Input '" + input_uri + "': " + parser.formatError(e, new ErrorFormatter(showTraces = true)))
        false
      case Failure(e) ⇒ System.err.println("Input '" + input_uri + "': Unexpected error during parsing run: " + e)
        false
    }
  }
}
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


import java.net.URLEncoder

import org.arktos.URIParser.URI_Map
import org.parboiled2.{ErrorFormatter, ParseError}
import org.scalatest.FlatSpec

import scala.util.{Failure, Success}


class arktosSpec extends FlatSpec {

  def testURI(input_uri: String) = {
    val ms: Double = System.currentTimeMillis
    val parser = URIParser(input_uri)
    val res = parser.URI_reference.run() match {
      case Success(x) ⇒
        val uri = ((new evalURI).eval(x): @unchecked) match {
          case URI_Map(x) ⇒ x
        }

        val me: Double = System.currentTimeMillis - ms
        System.err.println("Used time " + (me / 1000.0))

        val protocol = (uri.getOrElse("protocol", Left("")) : @unchecked) match { case Left(v) => v }
        val scheme = if ( protocol == "") ((uri.getOrElse("scheme", Left("")) : @unchecked) match { case Left(v) => v }) else protocol

        val path_list = (uri.getOrElse("params", Right(List())) : @unchecked) match { case Right(v) => v }
        val path = path_list.map({ case (k, null) => k; case (k, v) => k + "=" + URLEncoder.encode(v, "UTF-8") }).mkString("&")

        val uri_synthesized = scheme + (if ( scheme.length > 0 ) ":" + ((uri.getOrElse("scheme_postfix", Left("")): @unchecked) match { case Left(v) => v }) else "") +
          ((uri.getOrElse("authority", Left("")): @unchecked) match { case Left(v) => v }) +
          ((uri.getOrElse("path", Left("")) : @unchecked) match { case Left(v) => v }) +
          (if (path.length > 0) "?" + path else "") +
          ((uri.getOrElse("hash", Left("")) : @unchecked) match { case Left(v) => v })

        assert(input_uri == uri_synthesized)
      case Failure(e: ParseError) ⇒ System.err.println("Input '" + input_uri + "': " + parser.formatError(e, new ErrorFormatter(showTraces = true)))
        assert(false, e)
      case Failure(e) ⇒ System.err.println("Input '" + input_uri + "': Unexpected error during parsing run: " + e)
        assert(false, e)
    }
  }

  """The URI 'http://jp:secret@www.ietf.org/rfc/rfc2396.txt?p=1&p=URI#content'""" must "succeed" taggedAs (rfc3986) in {
    testURI( """http://jp:secret@www.ietf.org/rfc/rfc2396.txt?p=1&p=URI#content""")
  }

  """The URI 'urn:isbn:0451450523'""" must "succeed" taggedAs (rfc3986) in {
    testURI( """urn:isbn:0451450523""")
  }

  """The URI 'urn:lex:eu:council:directive:2010-03-09;2010-19-UE'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""urn:lex:eu:council:directive:2010-03-09;2010-19-UE""")
  }

  """The URI 'ftp://ftp.is.co.za/rfc/rfc1808.txt'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""ftp://ftp.is.co.za/rfc/rfc1808.txt""")
  }

  """The URI 'http://www.ietf.org/rfc/rfc2396.txt'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""http://www.ietf.org/rfc/rfc2396.txt""")
  }

  """The URI 'ldap://[2001:db8::7]/c=GB?objectClass?one'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""ldap://[2001:db8::7]/c=GB?objectClass?one""")
  }

  """The URI 'mailto:John.Doe@example.com'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""mailto:John.Doe@example.com""")
  }

  """The URI 'news:comp.infosystems.www.servers.unix'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""news:comp.infosystems.www.servers.unix""")
  }

  """The URI 'tel:+1-816-555-1212'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""tel:+1-816-555-1212""")
  }

  """The URI 'telnet://192.0.2.16:80/'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""telnet://192.0.2.16:80/""")
  }

  """The URI 'foo://example.com:8042/over/there?name=ferret#nose'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""foo://example.com:8042/over/there?name=ferret#nose""")
  }

  """The URI 'urn:oasis:names:specification:docbook:dtd:xml:4.1.2'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""urn:oasis:names:specification:docbook:dtd:xml:4.1.2""")
  }

  """The URI 'http://www.amazon.de/s/ref=nb_sb_noss/279-9128198-5070906?__mk_de_DE=%C3%85M%C3%85%C5%BD%C3%95%C3%91&url=search-alias%3Daps&field-keywords=scala+odersky'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""http://www.amazon.de/s/ref=nb_sb_noss/279-9128198-5070906?__mk_de_DE=%C3%85M%C3%85%C5%BD%C3%95%C3%91&url=search-alias%3Daps&field-keywords=scala%2Bodersky""")
  }

  """The URI 'http://[2001:0db8:85a3:08d3:1319:8a2e:0370:7344]:8080/'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""http://[2001:0db8:85a3:08d3:1319:8a2e:0370:7344]:8080/""")
  }

  """The URI 'http://[2002:4559:1FE2::4559:1FE2]:8080/'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""http://[2002:4559:1FE2::4559:1FE2]:8080/""")
  }

  """The URI 'http://[2002:4559:1FE2:0:0:0:4559:1FE2]:8080/'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""http://[2002:4559:1FE2:0:0:0:4559:1FE2]:8080/""")
  }

  """The URI 'http://[2002:4559:1FE2:0000:0000:0000:4559:1FE2]:8080/'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""http://[2002:4559:1FE2:0000:0000:0000:4559:1FE2]:8080/""")
  }

  """The URI 'http://2002:4559:1FE2:0000:0000:0000:4559:1FE2:8080/'""" must "fail(Bracketless IPV6 Address)" taggedAs (rfc3986) in {
    testURI("""http://2002:4559:1FE2:0000:0000:0000:4559:1FE2:8080/""")
  }

  """The URI 'http://[2001:cdba:0000:0000:0000:0000:3257:9652]/'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""http://[2001:cdba:0000:0000:0000:0000:3257:9652]/""")
  }

  """The URI 'http://[2001:cdba:0:0:0:0:3257:9652]/'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""http://[2001:cdba:0:0:0:0:3257:9652]/""")
  }

  """The URI 'http://[2001:cdba::3257:9652]/'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""http://[2001:cdba::3257:9652]/""")
  }


  """The URI 'http://[2001:0000:3238:DFE1:0063:0000:0000:FEFB]/example.com/in?out?test=off#link'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""http://[2001:0000:3238:DFE1:0063:0000:0000:FEFB]/example.com/in?out?test=off#link""")
  }

  """The URI 'http://[2001:0000:3238:DFE1:63:0000:0000:FEFB]/example.com/in?out?test=off#link'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""http://[2001:0000:3238:DFE1:63:0000:0000:FEFB]/example.com/in?out?test=off#link""")
  }

  """The URI 'http://[2001:0000:3238:DFE1:63::FEFB]/example.com/in?out?test=off#link'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""http://[2001:0000:3238:DFE1:63::FEFB]/example.com/in?out?test=off#link""")
  }

  """The URI 'http://[2001:0:3238:DFE1:63::FEFB]/example.com/in?out?test=off#link'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""http://[2001:0:3238:DFE1:63::FEFB]/example.com/in?out?test=off#link""")
  }

  """The URI 'http://[::]/'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""http://[::]/""")
  }

  """The URI 'http://[::128]/'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""http://[::128]/""")
  }
}
/*
* Copyright (C) 2015 - 2016 Juergen Pfundt
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

import org.scalatest.FlatSpec

class URISpecJVM extends FlatSpec {

  val uriencoder = new URIEncoder()

  def testURI(input: String, outcome: Boolean = true) = {

    val parsed_uri = URIParser(input)

    if (parsed_uri.isFailure) {
      System.err.println("Input '" + input + "': " + parsed_uri.failed.get)
    } else {
      val uri = parsed_uri.get

      val uri_synthesized = URI.build(uri)

      assert(input == uri_synthesized)
      assert(outcome)
    }
  }

  """The URI 'http://jp:secret@www.ietf.org/rfc/rfc2396.txt?p=1&p=URI#content'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""http://jp:secret@www.ietf.org/rfc/rfc2396.txt?p=1&p=URI#content""")
  }

  """The URI 'urn:isbn:0451450523'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""urn:isbn:0451450523""")
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

  """The URI 'tel:+1-617-253-5702'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""tel:+1-617-253-5702""")
  }

  """The URI 'fax:+1-617-258-5999'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""fax:+1-617-258-5999""")
  }

  """The URI 'mailto:timbl@w3.org'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""mailto:timbl@w3.org""")
  }

  """The URI 'http://www.w3.org/People/Berners-Lee/'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""http://www.w3.org/People/Berners-Lee/""")
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
    testURI("""http://www.amazon.de/s/ref=nb_sb_noss/279-9128198-5070906?__mk_de_DE=%C3%85M%C3%85%C5%BD%C3%95%C3%91&url=search-alias%3Daps&field-keywords=scala+odersky""", true)
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

  """The URI 'http://2002:4559:1FE2:0000:0000:0000:4559:1FE2:8080/'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""http://2002:4559:1FE2:0000:0000:0000:4559:1FE2:8080/""", false)
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

  """The URI 'http://[2001:db8:0:0:1:0:0:1]/example.com/in?out?test=off#link'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""http://[2001:db8:0:0:1:0:0:1]/example.com/in?out?test=off#link""")
  }

  """The URI 'http://[2001:0db8:0:0:1:0:0:1]/example.com/in?out?test=off#link'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""http://[2001:0db8:0:0:1:0:0:1]/example.com/in?out?test=off#link""")
  }

  """The URI 'http://[2001:db8::1:0:0:1]/example.com/in?out?test=off#link'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""http://[2001:db8::1:0:0:1]/example.com/in?out?test=off#link""")
  }

  """The URI 'http://[2001:db8::0:1:0:0:1]/example.com/in?out?test=off#link'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""http://[2001:db8::0:1:0:0:1]/example.com/in?out?test=off#link""")
  }

  """The URI 'http://[2001:0db8::1:0:0:1]/example.com/in?out?test=off#link'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""http://[2001:0db8::1:0:0:1]/example.com/in?out?test=off#link""")
  }

  """The URI 'http://[2001:db8:0:0:1::1]/example.com/in?out?test=off#link'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""http://[2001:db8:0:0:1::1]/example.com/in?out?test=off#link""")
  }

  """The URI 'http://[2001:db8:0000:0:1::1]/example.com/in?out?test=off#link'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""http://[2001:db8:0000:0:1::1]/example.com/in?out?test=off#link""")
  }

  """The URI 'http://[2001:DB8:0:0:1::1]/example.com/in?out?test=off#link'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""http://[2001:DB8:0:0:1::1]/example.com/in?out?test=off#link""")
  }

  """The URI 'http://[::]/'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""http://[::]/""")
  }

  """The URI 'http://[::128]/'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""http://[::128]/""")
  }

  """The URI 'http://[2441:4880:28:3:204:76ff:f43f:6eb]:8080'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""http://[2441:4880:28:3:204:76ff:f43f:6eb]:8080""")
  }

  """The URI 'http://[fe80::bd0f:a8bc:6480:238b%11]:8080'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""http://[fe80::bd0f:a8bc:6480:238b%11]:8080""")
  }

  """The URI 'http://[ff08::9abc%10]?a'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""http://[ff08::9abc%10]?a""")
  }

  """The URI 'xmlrpc.beep://stateserver.example.com/NumberToName'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""xmlrpc.beep://stateserver.example.com/NumberToName""")
  }

  """The URI 'xmlrpc.beep://10.0.0.2:1026'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""xmlrpc.beep://10.0.0.2:1026""")
  }

  """The URI 'http://de.wikipedia.org/wiki/Uniform_Resource_Identifier'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""http://de.wikipedia.org/wiki/Uniform_Resource_Identifier""")
  }

  """The URI 'file:///C:/Users/Benutzer/Desktop/Uniform%20Resource%20Identifier.html'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""file:///C:/Users/Benutzer/Desktop/Uniform%20Resource%20Identifier.html""")
  }

  """The URI 'file:///etc/fstab'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""file:///etc/fstab""")
  }

  """The URI 'geo:48.33,14.122;u=22.5'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""geo:48.33,14.122;u=22.5""")
  }

  """The URI 'gopher://gopher.floodgap.com'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""gopher://gopher.floodgap.com""")
  }

  """The URI 'sip:911@pbx.mycompany.com'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""sip:911@pbx.mycompany.com""")
  }

  """The URI 'data:text/plain;charset=iso-8859-7,%be%fa%be'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""data:text/plain;charset=iso-8859-7,%be%fa%be""")
  }

  """The URI 'git://github.com/rails/rails.giturn:oasis:names:specification:docbook:dtd:xml:4.1.2'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""git://github.com/rails/rails.git""")
  }

  """The URI 'crid://broadcaster.com/movies/BestActionMovieEver'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""crid://broadcaster.com/movies/BestActionMovieEver""")
  }

  """The URI 'https://unicode.com/encoding/char?Unicorn=%F0%9F%A6%84'""" must "succeed" taggedAs (rfc3986) in {
    testURI("""https://unicode.com/encoding/char?Unicorn=%F0%9F%A6%84""")
  }
}
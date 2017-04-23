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

import org.arktos.URI.ParamsListType
import org.scalatest.FlatSpec

class IRISpecJVM extends FlatSpec {
  val uriencoder = new URIEncoder()

  def testIRI(input: String, expected: String, outcome: Boolean = true) = {

    val parsed_iri = IRIParser(input)

    if (parsed_iri.isFailure) {
      System.err.println("Input '" + input + "': " + parsed_iri.failed.get)
    } else {

      val iri = parsed_iri.get

      // synthesize URI from parsed data

      // get protocol
      val protocol = iri.getOrElse("protocol", "").toString

      // if protocol is unknown ("") get scheme
      val `scheme`: String = if (protocol != "") protocol else  iri("scheme").toString
      // get parameters
      val params = iri.getOrElse("params", List()).asInstanceOf[ParamsListType]

      // rebuild parameter list
      val params_list = params.map({ case (k, None) ⇒ k; case (k, Some(v)) ⇒ k + "=" + v }).mkString("&")

      // assemble original uri
      val iri_synthesized = scheme + (if (`scheme`.length > 0) ":") + iri.getOrElse("scheme_postfix", "") +
        iri.getOrElse("authority", "") +
        iri.getOrElse("path", "") +
        (if (params_list.nonEmpty) "?" + params_list else "") +
        iri.getOrElse("hash", "")

      assert(expected == iri_synthesized)
      assert(outcome)
    }
  }

  """The IRI 'http://www.w%33.org'""" must "succeed" taggedAs (rfc3987) in {
    testIRI("""http://www.w%33.org""", "http://www.w3.org")
  }

  """The IRI 'http://r%C3%A4ksm%C3%B6rg%C3%A5s.josefsson.org'""" must "succeed" taggedAs (rfc3987) in {
    testIRI("""http://r%C3%A4ksm%C3%B6rg%C3%A5s.josefsson.org""", "http://räksmörgås.josefsson.org")
  }

  """The IRI 'http://%E7%B4%8D%E8%B1%86.w3.mag.keio.ac.jp'""" must "succeed" taggedAs (rfc3987) in {
    testIRI("""http://%E7%B4%8D%E8%B1%86.w3.mag.keio.ac.jp""", "http://納豆.w3.mag.keio.ac.jp")
  }

  """The IRI 'http://納豆.w3.mag.keio.ac.jp'""" must "succeed" taggedAs (rfc3987) in {
    testIRI("""http://納豆.w3.mag.keio.ac.jp""", "http://納豆.w3.mag.keio.ac.jp")
  }

  """The IRI 'http://www.%E3%81%BB%E3%82%93%E3%81%A8%E3%81%86%E3%81%AB%E3%81%AA%E3%81%8C%E3%81%84%E3%82%8F%E3%81%91%E3%81%AE%E3%82%8F%E3%81%8B%E3%82%89%E3%81%AA%E3%81%84%E3%81%A9%E3%82%81%E3%81%84%E3%82%93%E3%82%81%E3%81%84%E3%81%AE%E3%82%89%E3%81%B9%E3%82%8B%E3%81%BE%E3%81%A0%E3%81%AA%E3%81%8C%E3%81%8F%E3%81%97%E3%81%AA%E3%81%84%E3%81%A8%E3%81%9F%E3%82%8A%E3%81%AA%E3%81%84.w3.mag.keio.ac.jp/'""" must "succeed" taggedAs (rfc3987) in {
    testIRI("""http://www.%E3%81%BB%E3%82%93%E3%81%A8%E3%81%86%E3%81%AB%E3%81%AA%E3%81%8C%E3%81%84%E3%82%8F%E3%81%91%E3%81%AE%E3%82%8F%E3%81%8B%E3%82%89%E3%81%AA%E3%81%84%E3%81%A9%E3%82%81%E3%81%84%E3%82%93%E3%82%81%E3%81%84%E3%81%AE%E3%82%89%E3%81%B9%E3%82%8B%E3%81%BE%E3%81%A0%E3%81%AA%E3%81%8C%E3%81%8F%E3%81%97%E3%81%AA%E3%81%84%E3%81%A8%E3%81%9F%E3%82%8A%E3%81%AA%E3%81%84.w3.mag.keio.ac.jp/""", "http://www.ほんとうにながいわけのわからないどめいんめいのらべるまだながくしないとたりない.w3.mag.keio.ac.jp/")
  }

  """The IRI 'http://%E3%81%BB%E3%82%93%E3%81%A8%E3%81%86%E3%81%AB%E3%81%AA%E3%81%8C%E3%81%84%E3%82%8F%E3%81%91%E3%81%AE%E3%82%8F%E3%81%8B%E3%82%89%E3%81%AA%E3%81%84%E3%81%A9%E3%82%81%E3%81%84%E3%82%93%E3%82%81%E3%81%84%E3%81%AE%E3%82%89%E3%81%B9%E3%82%8B%E3%81%BE%E3%81%A0%E3%81%AA%E3%81%8C%E3%81%8F%E3%81%97%E3%81%AA%E3%81%84%E3%81%A8%E3%81%9F%E3%82%8A%E3%81%AA%E3%81%84.%E3%81%BB%E3%82%93%E3%81%A8%E3%81%86%E3%81%AB%E3%81%AA%E3%81%8C%E3%81%84%E3%82%8F%E3%81%91%E3%81%AE%E3%82%8F%E3%81%8B%E3%82%89%E3%81%AA%E3%81%84%E3%81%A9%E3%82%81%E3%81%84%E3%82%93%E3%82%81%E3%81%84%E3%81%AE%E3%82%89%E3%81%B9%E3%82%8B%E3%81%BE%E3%81%A0%E3%81%AA%E3%81%8C%E3%81%8F%E3%81%97%E3%81%AA%E3%81%84%E3%81%A8%E3%81%9F%E3%82%8A%E3%81%AA%E3%81%84.%E3%81%BB%E3%82%93%E3%81%A8%E3%81%86%E3%81%AB%E3%81%AA%E3%81%8C%E3%81%84%E3%82%8F%E3%81%91%E3%81%AE%E3%82%8F%E3%81%8B%E3%82%89%E3%81%AA%E3%81%84%E3%81%A9%E3%82%81%E3%81%84%E3%82%93%E3%82%81%E3%81%84%E3%81%AE%E3%82%89%E3%81%B9%E3%82%8B%E3%81%BE%E3%81%A0%E3%81%AA%E3%81%8C%E3%81%8F%E3%81%97%E3%81%AA%E3%81%84%E3%81%A8%E3%81%9F%E3%82%8A%E3%81%AA%E3%81%84.w3.mag.keio.ac.jp/'""" must "succeed" taggedAs (rfc3987) in {
    testIRI("""http://%E3%81%BB%E3%82%93%E3%81%A8%E3%81%86%E3%81%AB%E3%81%AA%E3%81%8C%E3%81%84%E3%82%8F%E3%81%91%E3%81%AE%E3%82%8F%E3%81%8B%E3%82%89%E3%81%AA%E3%81%84%E3%81%A9%E3%82%81%E3%81%84%E3%82%93%E3%82%81%E3%81%84%E3%81%AE%E3%82%89%E3%81%B9%E3%82%8B%E3%81%BE%E3%81%A0%E3%81%AA%E3%81%8C%E3%81%8F%E3%81%97%E3%81%AA%E3%81%84%E3%81%A8%E3%81%9F%E3%82%8A%E3%81%AA%E3%81%84.%E3%81%BB%E3%82%93%E3%81%A8%E3%81%86%E3%81%AB%E3%81%AA%E3%81%8C%E3%81%84%E3%82%8F%E3%81%91%E3%81%AE%E3%82%8F%E3%81%8B%E3%82%89%E3%81%AA%E3%81%84%E3%81%A9%E3%82%81%E3%81%84%E3%82%93%E3%82%81%E3%81%84%E3%81%AE%E3%82%89%E3%81%B9%E3%82%8B%E3%81%BE%E3%81%A0%E3%81%AA%E3%81%8C%E3%81%8F%E3%81%97%E3%81%AA%E3%81%84%E3%81%A8%E3%81%9F%E3%82%8A%E3%81%AA%E3%81%84.%E3%81%BB%E3%82%93%E3%81%A8%E3%81%86%E3%81%AB%E3%81%AA%E3%81%8C%E3%81%84%E3%82%8F%E3%81%91%E3%81%AE%E3%82%8F%E3%81%8B%E3%82%89%E3%81%AA%E3%81%84%E3%81%A9%E3%82%81%E3%81%84%E3%82%93%E3%82%81%E3%81%84%E3%81%AE%E3%82%89%E3%81%B9%E3%82%8B%E3%81%BE%E3%81%A0%E3%81%AA%E3%81%8C%E3%81%8F%E3%81%97%E3%81%AA%E3%81%84%E3%81%A8%E3%81%9F%E3%82%8A%E3%81%AA%E3%81%84.w3.mag.keio.ac.jp/""", "http://ほんとうにながいわけのわからないどめいんめいのらべるまだながくしないとたりない.ほんとうにながいわけのわからないどめいんめいのらべるまだながくしないとたりない.ほんとうにながいわけのわからないどめいんめいのらべるまだながくしないとたりない.w3.mag.keio.ac.jp/")
  }

  """The URI 'https://unicode.com/encoding/char?Unicorn=Test%F0%9F%A6%84Char'""" must "succeed" taggedAs (rfc3987) in {
    testIRI("""https://unicode.com/encoding/char?Unicorn=Test%F0%9F%A6%84Char""", """https://unicode.com/encoding/char?Unicorn=Test🦄Char""")
  }

  """The URI 'https://unicode.com/encoding/char?Unicorn=Test🦄Char'""" must "succeed" taggedAs (rfc3987) in {
    testIRI("""https://unicode.com/encoding/char?Unicorn=Test🦄Char""", """https://unicode.com/encoding/char?Unicorn=Test🦄Char""")
  }

  """The URI 'http://www.amazon.de/s/ref=nb_sb_noss/279-9128198-5070906?__mk_de_DE=%C3%85M%C3%85%C5%BD%C3%95%C3%91&url=search-alias%3Daps&field-keywords=scala+odersky'""" must "succeed" taggedAs (rfc3986) in {
    testIRI("""http://www.amazon.de/s/ref=nb_sb_noss/279-9128198-5070906?__mk_de_DE=%C3%85M%C3%85%C5%BD%C3%95%C3%91&url=search-alias%3Daps&field-keywords=scala+odersky""", """http://www.amazon.de/s/ref=nb_sb_noss/279-9128198-5070906?__mk_de_DE=ÅMÅŽÕÑ&url=search-alias=aps&field-keywords=scala+odersky""")
  }

  """The URI 'http://www.amazon.de/s/ref=nb_sb_noss/279-9128198-5070906?__mk_de_DE=ÅMÅŽÕÑ&url=search-alias%3Daps&field-keywords=scala+odersky'""" must "succeed" taggedAs (rfc3986) in {
    testIRI("""http://www.amazon.de/s/ref=nb_sb_noss/279-9128198-5070906?__mk_de_DE=ÅMÅŽÕÑ&url=search-alias=aps&field-keywords=scala+odersky""", """http://www.amazon.de/s/ref=nb_sb_noss/279-9128198-5070906?__mk_de_DE=ÅMÅŽÕÑ&url=search-alias=aps&field-keywords=scala+odersky""")
  }
}

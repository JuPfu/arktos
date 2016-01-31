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

import java.io.ByteArrayOutputStream

/*
 * Utility class for URI decoding.
 *
 * The conversion process is the reverse of that used by the URIEncoder class.
 *
 * The rules of the RFC3986 URIParser assure the correctness of the syntax for hex-encoded characters passed as
 * arguments to the decode function of this class. The input string contains only legal characters and correctly formed
 * pct-encoded characters (pct-encoded = "%" HEXDIG HEXDIG). Therefore no validation or sanity checks are part of
 * the decode function.
 */

object URIDecoder {
  def apply = new URIDecoder()
}

class URIDecoder {

  def decode(s: String, charset: String = "UTF-8"): String = {

    def PCTEncodedOctetToNibble(c: Char) = if (c <= '9') c - '0' else if (c <= 'F') c - '7' else c - 'W'

    val bos = new ByteArrayOutputStream(1024)

    val iterator = s.iterator

    while (iterator.hasNext) {
      val c: Char = iterator.next()

      if (c != '%') {
        val it: Iterator[Byte] = c.toString.getBytes(charset).iterator
        while (it.hasNext) bos.write(it.next())

        if (c.isSurrogate) {
          val it: Iterator[Byte] = c.toString.getBytes(charset).iterator
          while (it.hasNext) bos.write(it.next())
        }
      } else {
        bos.write(PCTEncodedOctetToNibble(iterator.next()) << 4 | PCTEncodedOctetToNibble(iterator.next()))
      }
    }
    bos.toString(charset)
  }
}

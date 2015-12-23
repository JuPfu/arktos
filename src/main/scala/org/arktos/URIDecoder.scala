/*
 * Copyright (C) 2015 Juergen Pfundt
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
 * The rules of the RFC3986 URIParser assure the correctness of the syntax for hex-encoded characters.
 *
 */

object URIDecoder {
  def apply = new URIDecoder()
}

class URIDecoder {

  final val NOT_ENCODED = 0
  final val PCT_ENCODED_HIGH = 1
  final val PCT_ENCODED_LOW = 2

  def decode(s: String): String = {

    val bos = new ByteArrayOutputStream()

    var high_byte = 0

    var status = NOT_ENCODED

    val iterator = s.iterator

    while (iterator.hasNext) {
      val c = iterator.next()
      if (status == NOT_ENCODED) {
        if (c != '%')
          bos.write(c)
        else
          status = PCT_ENCODED_HIGH
      } else if (status == PCT_ENCODED_HIGH) {
        if (c <= '9')
          high_byte = c - '0'
        else if (c <= 'F')
          high_byte = c - '7'
        else
          high_byte = c - 'W'

        status = PCT_ENCODED_LOW
      } else if (status == PCT_ENCODED_LOW) {

        bos.write((high_byte << 4 | (if (c <= '9') c - '0' else if (c <= 'F') c - '7' else c - 'W')))

        status = NOT_ENCODED
      }
    }
    bos.toString("UTF-8")
  }
}

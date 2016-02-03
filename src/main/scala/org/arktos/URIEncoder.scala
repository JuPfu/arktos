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

import scala.annotation.tailrec

/*
 * Utility class for URI encoding.
 *
 * The conversion process is the reverse of that used by the URIDecoder class.
 *
 */

/*
excerpt from http://www.rfc-base.org/txt/rfc-3986.txt

2.4 When to Encode or Decode

Under normal circumstances, the only time when octets within a URI
are percent-encoded is during the process of producing the URI from
its component parts.  This is when an implementation determines which
of the reserved characters are to be used as subcomponent delimiters
and which can be safely used as data.  Once produced, a URI is always
in its percent-encoded form.

When a URI is dereferenced, the components and subcomponents
significant to the scheme-specific dereferencing process (if any)
must be parsed and separated before the percent-encoded octets within
those components can be safely decoded, as otherwise the data may be
mistaken for component delimiters.  The only exception is for
percent-encoded octets corresponding to characters in the unreserved
set, which can be decoded at any time.  For example, the octet
corresponding to the tilde ("~") character is often encoded as "%7E"
by older URI processing implementations; the "%7E" can be replaced by
"~" without changing its interpretation.

Because the percent ("%") character serves as the indicator for
percent-encoded octets, it must be percent-encoded as "%25" for that
octet to be used as data within a URI.  Implementations must not
percent-encode or decode the same string more than once, as decoding
an already decoded string might lead to misinterpreting a percent
data octet as the beginning of a percent-encoding, or vice versa in
the case of percent-encoding an already percent-encoded string.
*/

object URIEncoder {
  def apply = new URIEncoder()

  val hexmap = Array('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F')
}

class URIEncoder {

  def encode(s: String, charset: String = "UTF-8"): String = {

    val iterator: Iterator[Byte] = s.getBytes(charset).iterator

    val bos = new ByteArrayOutputStream(1024)

    def writeHexEncodedCharPart(b: Byte) = {
      import URIEncoder.hexmap

      bos.write('%')
      bos.write(hexmap((b >> 4) & 0x0F))
      bos.write(hexmap(b & 0x0F))
    }

    @tailrec
    def writeHexRepresentationOfMultiByteChar(byte: Byte, count: Int): Unit = {
      if (((byte << count) & 0x80) != 0) {
        writeHexEncodedCharPart(iterator.next)
        writeHexRepresentationOfMultiByteChar(byte, count + 1)
      }
    }

    while (iterator.hasNext) {
      val byte = iterator.next

      if ((byte & 0x80) == 0 &&
        ((byte >= '0' && byte <= '9') ||
          (byte >= 'A' && byte <= 'Z') ||
          (byte >= 'a' && byte <= 'z') ||
          byte == '-' ||
          byte == '.' ||
          byte == '_' ||
          byte == '~')) {
        /* write single byte character encoded into two
         * hexadecimal characters (first nibble and second nibble) to ByteArrayOutputStream
         */
        bos.write(byte)
      } else {
        /* write first byte of a multi-byte character encoded into two
         * hexadecimal characters (first nibble and second nibble) to ByteArrayOutputStream
         */
        writeHexEncodedCharPart(byte)
        /* recursively write all remaining bytes of a multi-byte character
         * each byte encoded into two hexadecimal characters (first nibble and second nibble)
         * to ByteArrayOutputStream */
        writeHexRepresentationOfMultiByteChar(byte, 1)
      }
    }
    bos.toString("UTF-8")
  }
}

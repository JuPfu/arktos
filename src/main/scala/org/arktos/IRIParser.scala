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

import org.arktos.URIParser._
import org.parboiled2.{ ErrorFormatter, ParseError, ParserInput, CharPredicate }
import org.parboiled2.CharPredicate._

import scala.util.{ Failure, Success }

class IRIParser(input: ParserInput) extends URIParser(input: ParserInput) {

  import IRIParser._

  val ucschar = CharPredicate('\u00A0' to '\uD7FF', '\uF900' to '\uFDCF', '\uFDF0' to '\uFFEF')
  //private       = %xE000-F8FF / %xF0000-FFFFD / %x100000-10FFFD
  val `private` = CharPredicate('\uE000' to '\uF8FF')

  // unreserved     = ALPHA / DIGIT / "-" / "." / "_" / "~" / ucschar
  override val unreserved = AlphaNum ++ '-' ++ '.' ++ '_' ++ '~' ++ ucschar

  def private_supplement = rule { `private` | !(str("\\U000FFFFE") | str("\\U000FFFFF") | str("\\U00010FFE") | str("\\U00010FFF")) ~ isHighSurrogate ~ isLowSurrogate }

  def ucschar_supplement = rule {
    !(str("\\U0001FFFE") | str("\\U0001FFFF") |
      str("\\U0002FFFE") | str("\\U0002FFFF") |
      str("\\U0003FFFE") | str("\\U0003FFFF") |
      str("\\U0004FFFE") | str("\\U0004FFFF") |
      str("\\U0005FFFE") | str("\\U0005FFFF") |
      str("\\U0006FFFE") | str("\\U0006FFFF") |
      str("\\U0007FFFE") | str("\\U0007FFFF") |
      str("\\U0008FFFE") | str("\\U0008FFFF") |
      str("\\U0009FFFE") | str("\\U0009FFFF") |
      str("\\U000AFFFE") | str("\\U000AFFFF") |
      str("\\U000BFFFE") | str("\\U000BFFFF") |
      str("\\U000CFFFE") | str("\\U000CFFFF") |
      str("\\U000DFFFE") | str("\\U000DFFFF") |
      str("\\U000EFFFE") | str("\\U000EFFFF")) ~ isHighSurrogate ~ isLowSurrogate
  }

  // URI-reference = URI / relative-ref
  def IRI_reference = rule { (URI | relative_ref) ~ EOI ~> URI_Reference }

  // user          = *( unreserved / pct-encoded / sub-delims )
  override def user = rule {
    atomic(capture((unreserved | pct_encoded | sub_delims | ucschar_supplement).*)) ~> URI_User
  }

  // reg-name      = *( unreserved / pct-encoded / sub-delims )
  override def reg_name = rule { atomic(capture((unreserved | pct_encoded | sub_delims | ucschar_supplement).*)) ~> URI_Reg_Name }

  //segment-nz-nc = 1*( unreserved / pct-encoded / sub-delims / "@" ) ; non-zero-length segment without any ':' ":"
  override def segment_nz_nc = rule { (unreserved | pct_encoded | sub_delims | '@' | ucschar_supplement).+ }

  override def qchar = rule { unreserved | pct_encoded | query_delims | ':' | '@' | ucschar_supplement | `private` }
}

object IRIParser {

  val isHighSurrogate = CharPredicate.from(Character.isHighSurrogate)
  val isLowSurrogate = CharPredicate.from(Character.isLowSurrogate)

  def apply(input: ParserInput) = {

    val parser = new IRIParser(input)
    val result = parser.IRI_reference.run()

    result match {
      case Success(x)             ⇒ Success((new evalURI().eval(result.get): @unchecked) match { case URI_Map(x) ⇒ x })
      case Failure(e: ParseError) ⇒ Failure(new RuntimeException(parser.formatError(result.failed.get.asInstanceOf[org.parboiled2.ParseError], new ErrorFormatter())))
      case Failure(e)             ⇒ Failure(new RuntimeException("Unexpected error during parsing run: " + result.failed.get))
    }
  }
}

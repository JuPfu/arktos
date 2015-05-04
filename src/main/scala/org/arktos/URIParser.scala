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

/* This Parboiled2 grammar implements the

   Uniform Resource Identifier (URI): Generic Syntax

   as described in

   http://www.ietf.org/rfc/rfc3986.txt

   Network Working Group                                     T. Berners-Lee
   Request for Comments: 3986                                       W3C/MIT
   STD: 66                                                      R. Fielding
   Updates: 1738                                               Day Software
   Obsoletes: 2732, 2396, 1808                                  L. Masinter
   Category: Standards Track                                  Adobe Systems
                                                               January 2005
*/

package org.arktos

import org.parboiled2.{ ErrorFormatter, ParseError, ParserInput, _ }

import scala.util.{ Failure, Success }

class URIParser(val input: ParserInput) extends Parser with StringBuilding {

  import CharPredicate.{ Alpha, AlphaNum, Digit, Digit19, HexDigit }
  import URIParser._

  // dec_octet helpers
  val Digit4 = CharPredicate('0' to '4')
  val Digit5 = Digit4 ++ '5'
  // ALPHA / DIGIT / "+" / "-" / "."
  val scheme_char = AlphaNum ++ '+' ++ '-' ++ '.'
  // unreserved     = ALPHA / DIGIT / "-" / "." / "_" / "~"
  val unreserved = AlphaNum ++ '-' ++ '.' ++ '_' ++ '~'
  // gen-delims     = ":" / "/" / "?" / "#" / "[" / "]" / "@"
  val gen_delims = CharPredicate(':', '/', '?', '#', '[', ']', '@')
  // sub-delims = "!" / "$" / "&" / "'" / "(" / ")" / "*" / "+" / "," / ";" / "="
  val sub_delims = CharPredicate('!', '$', '&', '\'', '(', ')', '*', '+', ',', ';', '=')
  // query_delims = CharPredicate('!', '$', ''', '(', ')', '*', '+', ',', ';')
  val query_delims = CharPredicate('!', '$', '\'', '(', ')', '*', '+', ',', ';')
  // reserved      = gen-delims / sub-delims
  val reserved = gen_delims ++ sub_delims

  // URI           = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
  def URI = rule { scheme ~ ':' ~ hier_part ~ ('?' ~ query).? ~ ('#' ~ fragment).? ~> URI_URI }

  // hier-part     = "//" authority path-abempty / path-absolute / path-rootless / path-empty
  def hier_part = rule {
    ('/' ~ '/' ~ authority ~ path_abempty) ~> URI_Hier_Part_Absolute |
      (path_absolute | path_rootless | path_empty) ~> URI_Hier_Part_Path
  }

  // URI-reference = URI / relative-ref
  def URI_reference = rule { (URI | relative_ref) ~ EOI ~> URI_Reference }

  // absolute-URI  = scheme ":" hier-part [ "?" query ]
  def absolute_URI = rule { scheme ~ ':' ~ hier_part ~ ('?' ~ query).? ~> URI_AbsoluteURI }

  // relative-ref  = relative-part [ "?" query ] [ "#" fragment ]
  def relative_ref = rule { relative_part ~ ('?' ~ query).? ~ ('#' ~ fragment).? ~> URI_Relative_Ref }

  // relative-part = "//" authority path-abempty / path-absolute / path-noscheme / path-empty
  def relative_part = rule {
    ('/' ~ '/' ~ authority ~ path_abempty) ~> URI_Relative_Part |
      (path_absolute | path_noscheme | path_empty) ~> URI_Relative_Part_Path
  }

  // scheme        = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
  def scheme = rule { atomic(capture(Alpha ~ scheme_char.*)) ~> URI_Scheme }

  // authority     = [ userinfo "@" ] host [ ":" port ]
  def authority = rule { (userinfo ~ '@').? ~ host ~ (':' ~ port).? ~> URI_Authority }

  // userinfo      = user [ ":" password ]
  def userinfo = rule { user ~ (':' ~ password).? ~> URI_UserInfo }

  // user          = *( unreserved / pct-encoded / sub-delims )
  def user = rule { atomic(capture((unreserved | pct_encoded | sub_delims).*)) ~> URI_User }

  // password      = *( unreserved / pct-encoded / sub-delims / ":" )
  def password = rule { atomic(capture((unreserved | pct_encoded | sub_delims | ':').*)) ~> URI_Password }

  // host          = IP-literal / IPv4address / reg-name
  def host = rule { (IP_literal | IPv4address | reg_name) ~> URI_Host }

  // port          = *DIGIT
  def port = rule { atomic(capture(Digit.*)) ~> URI_Port }

  // IP-literal    = "[" ( IPv6address / IPvFuture  ) "]"
  def IP_literal = rule { '[' ~ (IPv6address | IPvFuture) ~ ']' ~> URI_IP_Literal }

  // IPvFuture     = "v" 1*HEXDIG "." 1*( unreserved / sub-delims / ":" )
  def IPvFuture = rule { atomic(capture('v' ~ HexDigit ~ '.' ~ (unreserved | sub_delims | ':'))) ~> URI_IPvFuture }

  // IPv6address   =                            6( h16 ":" ) ls32
  //               /                       "::" 5( h16 ":" ) ls32
  //               / [               h16 ] "::" 4( h16 ":" ) ls32
  //               / [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
  //               / [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
  //               / [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32
  //               / [ *4( h16 ":" ) h16 ] "::"              ls32
  //               / [ *5( h16 ":" ) h16 ] "::"              h16
  //               / [ *6( h16 ":" ) h16 ] "::"
  def IPv6address = rule {
    atomic((capture(6.times(h16 ~ ':') ~ ls32) |
      capture(':' ~ ':' ~ 5.times(h16 ~ ':') ~ ls32) |
      capture(h16.? ~ ':' ~ ':' ~ 4.times(h16 ~ ':') ~ ls32) |
      capture(((h16 ~ ':' ~ h16) | h16).? ~ ':' ~ ':' ~ 3.times(h16 ~ ':') ~ ls32) |
      capture(((1 to 2 times (h16 ~ ':') ~ h16) | h16).? ~ ':' ~ ':' ~ 2.times(h16 ~ ':') ~ ls32) |
      capture(((1 to 3 times (h16 ~ ':') ~ h16) | h16).? ~ ':' ~ ':' ~ h16 ~ ':' ~ ls32) |
      capture(((1 to 4 times (h16 ~ ':') ~ h16) | h16).? ~ ':' ~ ':' ~ ls32) |
      capture(((1 to 5 times (h16 ~ ':') ~ h16) | h16).? ~ ':' ~ ':' ~ h16) |
      capture(((1 to 6 times (h16 ~ ':') ~ h16) | h16).? ~ ':' ~ ':'))) ~> URI_IPv6Address
  }

  // h16           = 1*4HEXDIG
  def h16 = rule { 1 to 4 times (HexDigit) }

  // ls32          = ( h16 ":" h16 ) / IPv4address
  def ls32 = rule { h16 ~ ':' ~ h16 | dec_octet ~ '.' ~ dec_octet ~ '.' ~ dec_octet ~ '.' ~ dec_octet }

  // IPv4address   = dec-octet "." dec-octet "." dec-octet "." dec-octet
  def IPv4address = rule { atomic(capture(dec_octet ~ '.' ~ dec_octet ~ '.' ~ dec_octet ~ '.' ~ dec_octet)) ~> URI_IPv4Address }

  // dec-octet     = DIGIT                 ; 0-9
  //               / %x31-39 DIGIT         ; 10-99
  //               / "1" 2DIGIT            ; 100-199
  //               / "2" %x30-34 DIGIT     ; 200-249
  //               / "25" %x30-35          ; 250-255
  def dec_octet = rule { '1' ~ Digit ~ Digit | '2' ~ Digit4 ~ Digit | '2' ~ '5' ~ Digit5 | Digit19 ~ Digit | Digit }

  // reg-name      = *( unreserved / pct-encoded / sub-delims )
  def reg_name = rule { atomic(capture((unreserved | pct_encoded | sub_delims).*)) ~> URI_Reg_Name }

  // A registered name intended for lookup in the DNS uses the syntax
  // defined in Section 3.5 of [RFC1034] and Section 2.1 of [RFC1123].
  // Such a name consists of a sequence of domain labels separated by ".",
  // each domain label starting and ending with an alphanumeric character
  // and possibly also containing "-" characters.  The rightmost domain
  // label of a fully qualified domain name in DNS may be followed by a
  // single "." and should be if it is necessary to distinguish between
  // the complete domain name and some local domain.

  // def reg_name = rule { subdomain ~ domain }
  // def subdomain = rule { (reg_segment.? ~ '.'.+).? }
  // def domain = rule { (reg_segment.? ~ '.').* }

  // path          = path-abempty   ; begins with "/" or is empty
  //               / path-absolute  ; begins with "/" but not "//"
  //               / path-noscheme  ; begins with a non-':' segment
  //               / path-rootless  ; begins with a segment
  //               / path-empty     ; zero characters
  def path = rule { (path_absolute | path_noscheme | path_rootless | path_abempty | path_empty) ~> URI_Path }

  // path-abempty  = *( "/" segment )
  def path_abempty = rule { atomic(capture(('/' ~ segment).*)) ~> URI_Path_AbEmpty }

  // path-absolute = "/" [ segment-nz *( "/" segment ) ]
  def path_absolute = rule { atomic(capture('/' ~ (segment_nz ~ ('/' ~ segment).*).?)) ~> URI_Path_Absolute }

  // path-noscheme = segment-nz-nc *( "/" segment )
  def path_noscheme = rule { atomic(capture(segment_nz_nc ~ ('/' ~ segment).*)) ~> URI_Path_NoScheme }

  // path-rootless = segment-nz *( "/" segment )
  def path_rootless = rule { atomic(capture(segment_nz ~ ('/' ~ segment).*)) ~> URI_Path_Rootless }

  // path-empty    = 0<pchar>
  def path_empty = rule { capture("") ~> URI_Path_Empty }

  // segment       = *pchar
  def segment = rule { pchar.* }

  // segment-nz    = 1*pchar
  def segment_nz = rule { pchar.+ }

  //segment-nz-nc = 1*( unreserved / pct-encoded / sub-delims / "@" ) ; non-zero-length segment without any ':' ":"
  def segment_nz_nc = rule { (unreserved | pct_encoded | sub_delims | '@').+ }

  // pchar         = unreserved / pct-encoded / sub-delims / ":" / "@"
  def pchar = rule { unreserved | pct_encoded | sub_delims | ':' | '@' }

  // query         = *( ipchar / iprivate / "/" / "?" )
  // def query = rule { capture((pchar | '/' | '?').*) ~> URI_Query }
  def query = rule { (queryParameter | queryToken).*('&') ~> URI_Query }
  def queryParameter = rule { queryVariable ~ '=' ~ queryValue ~> URI_QueryParameter }
  def queryVariable = rule { capture((qchar | '/' | '?').*) ~> URI_QueryVariable }
  def queryValue = rule { capture((qchar | '/' | '?').*) ~> URI_QueryValue }
  def queryToken = rule { capture((qchar | '/' | '?').*) ~> URI_QueryToken }
  def qchar = rule { unreserved | pct_encoded | query_delims | ':' | '@' }

  // fragment      = *( pchar / "/" / "?" )
  def fragment = rule { atomic(capture((pchar | '/' | '?').*)) ~> URI_Fragment }

  // pct-encoded    = "%" HEXDIG HEXDIG
  def pct_encoded = rule { '%' ~ HexDigit ~ HexDigit }
}

object URIParser extends App {

  sealed trait URI_Return_Value
  case class URI_String(s: String) extends URI_Return_Value
  case class URI_Tuple(s: (String, String)) extends URI_Return_Value
  case class URI_Param(s: List[(String, String)]) extends URI_Return_Value
  case class URI_Map(m: Map[String, Either[String, List[(String, String)]]]) extends URI_Return_Value

  sealed trait URI_AST
  case class URI_URI(scheme: URI_Scheme, hier_part: URI_AST, query: Option[URI_Query], fragment: Option[URI_Fragment]) extends URI_AST
  case class URI_Hier_Part_Absolute(authority: URI_Authority, path: URI_AST) extends URI_AST
  case class URI_Hier_Part_Path(path: URI_AST) extends URI_AST
  case class URI_Reference(rule: URI_AST) extends URI_AST
  case class URI_AbsoluteURI(scheme: URI_Scheme, hier_part: URI_AST, query: Option[URI_Query]) extends URI_AST
  case class URI_Relative_Ref(relative_part: URI_AST, query: Option[URI_Query], fragment: Option[URI_Fragment]) extends URI_AST
  case class URI_Relative_Part(authority: URI_Authority, path: URI_AST) extends URI_AST
  case class URI_Relative_Part_Path(path: URI_AST) extends URI_AST
  case class URI_Scheme(scheme: String) extends URI_AST
  case class URI_Authority(userinfo: Option[URI_UserInfo], host: URI_Host, port: Option[URI_Port]) extends URI_AST
  case class URI_UserInfo(user: URI_User, password: Option[URI_Password]) extends URI_AST
  case class URI_User(user: String) extends URI_AST
  case class URI_Password(password: String) extends URI_AST
  case class URI_Host(rule: URI_AST) extends URI_AST
  case class URI_Port(port: String) extends URI_AST
  case class URI_IP_Literal(rule: URI_AST) extends URI_AST
  case class URI_IPvFuture(ipvfuture: String) extends URI_AST
  case class URI_IPv6Address(ipv6address: String) extends URI_AST
  case class URI_IPv4Address(ipv4address: String) extends URI_AST
  case class URI_Reg_Name(reg_name: String) extends URI_AST
  case class URI_Path(rule: URI_AST) extends URI_AST
  case class URI_Path_AbEmpty(path_abempty: String) extends URI_AST
  case class URI_Path_Absolute(path_absolute: String) extends URI_AST
  case class URI_Path_NoScheme(path_noscheme: String) extends URI_AST
  case class URI_Path_Rootless(path_rootless: String) extends URI_AST
  case class URI_Path_Empty(empty: String) extends URI_AST
  //case class URI_Query(query: String) extends URI_AST
  case class URI_Query(query: Seq[URI_AST]) extends URI_AST
  case class URI_QueryParameter(queryvariable: URI_QueryVariable, queryvalue: URI_QueryValue) extends URI_AST
  case class URI_QueryVariable(queryvariable: String) extends URI_AST
  case class URI_QueryValue(queryvariable: String) extends URI_AST
  case class URI_QueryToken(querytoken: String) extends URI_AST
  case class URI_Fragment(fragment: String) extends URI_AST
  case class Error(msg: String) extends URI_AST

  lazy val input: ParserInput = args(0)

  System.err.println("testing: " + args(0))

  URIParser(input)

  def apply(input: ParserInput) = {

    val parser = new URIParser(input)

    val res = parser.URI_reference.run()
    val eval_uri = new evalURI

    res match {
      case Success(x) ⇒
        val m = (eval_uri.eval(x): @unchecked) match { case URI_Map(x) ⇒ x }
        System.out.println("RESULT->" + m.mapValues { case Left(v) ⇒ v; case (Right(v)) ⇒ v })
      case Failure(e: ParseError) ⇒ System.err.println("Input '" + args(0) + "': " + parser.formatError(e, new ErrorFormatter(showTraces = true)))
      case Failure(e)             ⇒ System.err.println("Input '" + args(0) + "': Unexpected error during parsing run: " + e)
    }
  }

}

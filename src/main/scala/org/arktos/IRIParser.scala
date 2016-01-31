package org.arktos

import org.arktos.URIParser._
import org.parboiled2.{ ErrorFormatter, ParseError, ParserInput, CharPredicate }
import org.parboiled2.CharPredicate._

import scala.util.{ Failure, Success }

/**
 * Created by jp on 31.01.16.
 */
class IRIParser(input: ParserInput) extends URIParser(input: ParserInput) {

  import IRIParser._

  val ucschar = CharPredicate('\u00A0' to '\uD7FF', '\uF900' to '\uFDCF', '\uFDF0' to '\uFFEF')

  // unreserved     = ALPHA / DIGIT / "-" / "." / "_" / "~" / ucschar
  override val unreserved = AlphaNum ++ '-' ++ '.' ++ '_' ++ '~' ++ ucschar

  // URI-reference = URI / relative-ref
  def IRI_reference = rule { (URI | relative_ref) ~ EOI ~> URI_Reference }

  // user          = *( unreserved / pct-encoded / sub-delims )
  override def user = rule {
    atomic(capture((unreserved | pct_encoded | sub_delims |
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
        str("\\U000EFFFE") | str("\\U000EFFFF")) ~ isHighSurrogate ~ isLowSurrogate).*)) ~> URI_User
  }

  // reg-name      = *( unreserved / pct-encoded / sub-delims )
  override def reg_name = rule { atomic(capture((unreserved | pct_encoded | sub_delims | isHighSurrogate ~ isLowSurrogate).*)) ~> URI_Reg_Name }

  //segment-nz-nc = 1*( unreserved / pct-encoded / sub-delims / "@" ) ; non-zero-length segment without any ':' ":"
  override def segment_nz_nc = rule { (unreserved | pct_encoded | sub_delims | '@' | isHighSurrogate ~ isLowSurrogate).+ }

  override def qchar = rule { unreserved | pct_encoded | query_delims | ':' | '@' | isHighSurrogate ~ isLowSurrogate /* | supplement */ }
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

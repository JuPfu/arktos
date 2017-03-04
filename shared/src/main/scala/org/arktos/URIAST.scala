package org.arktos

/**
 * Created by jp on 02.03.17.
 */
sealed trait URIAST {
  case class URI_URI(scheme: URI_Scheme, hier_part: URIAST, query: Option[URI_Query], fragment: Option[URI_Fragment]) extends URIAST
  case class URI_Hier_Part_Absolute(authority: URI_Authority, path: URIAST) extends URIAST
  case class URI_Hier_Part_Path(path: URIAST) extends URIAST
  case class URI_Reference(rule: URIAST) extends URIAST
  case class URI_AbsoluteURI(scheme: URI_Scheme, hier_part: URIAST, query: Option[URI_Query]) extends URIAST
  case class URI_Relative_Ref(relative_part: URIAST, query: Option[URI_Query], fragment: Option[URI_Fragment]) extends URIAST
  case class URI_Relative_Part(authority: URI_Authority, path: URIAST) extends URIAST
  case class URI_Relative_Part_Path(path: URIAST) extends URIAST
  case class URI_Scheme(scheme: String) extends URIAST
  case class URI_Authority(userinfo: Option[URI_UserInfo], host: URI_Host, port: Option[URI_Port]) extends URIAST
  case class URI_UserInfo(user: URI_User, password: Option[URI_Password]) extends URIAST
  case class URI_User(user: String) extends URIAST
  case class URI_Password(password: String) extends URIAST
  case class URI_Host(rule: URIAST) extends URIAST
  case class URI_Port(port: String) extends URIAST
  case class URI_IP_Literal(rule: URIAST) extends URIAST
  case class URI_IPvFuture(ipvfuture: String) extends URIAST
  case class URI_IPvFutureLinkLocal(ipv6Address: URI_IPv6Address, zoneID: URI_ZoneID) extends URIAST
  case class URI_ZoneID(zoneid: String) extends URIAST
  case class URI_IPv6Address(ipv6address: String) extends URIAST
  case class URI_IPv6AddressZ(ipv6Address: URI_IPv6Address, zoneID: URI_ZoneID) extends URIAST
  case class URI_IPv4Address(ipv4address: String) extends URIAST
  case class URI_Reg_Name(reg_name: String) extends URIAST
  case class URI_Path(rule: URIAST) extends URIAST
  case class URI_Path_AbEmpty(path_abempty: Seq[String]) extends URIAST
  case class URI_Path_Absolute(path_absolute: Seq[String]) extends URIAST
  case class URI_Path_NoScheme(path_noscheme: Seq[String]) extends URIAST
  case class URI_Path_Rootless(path_rootless: Seq[String]) extends URIAST
  case class URI_Path_Empty(empty: String) extends URIAST
  case class URI_Query(query: Seq[URIAST]) extends URIAST
  case class URI_QueryParameter(queryvariable: URI_QueryVariable, queryvalue: URI_QueryValue) extends URIAST
  case class URI_QueryVariable(queryvariable: String) extends URIAST
  case class URI_QueryValue(queryvariable: String) extends URIAST
  case class URI_QueryToken(querytoken: String) extends URIAST
  case class URI_Fragment(fragment: String) extends URIAST
  case class Error(msg: String) extends URIAST
}

object URIAST extends URIAST
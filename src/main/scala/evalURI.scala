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
package org.uri

import org.uri.URIParser._

import scala.annotation.tailrec

object evalURI {
  def apply() = new evalURI()
}

class evalURI {

  def eval(expr: URI_AST): URI_Return_Value = {
    println("EVAL=" + expr)
    expr match {
      case URI_URI(scheme, hier_part, query, fragment) ⇒
        ((((eval(scheme),
          eval(hier_part),
          query match {
            case Some(q) ⇒ eval(q)
            case None ⇒ URI_String("")
          },
          fragment match {
            case Some(f) ⇒ eval(f)
            case None ⇒ URI_String("")
          }): @unchecked) match {
          case (URI_Map(m1), URI_Map(m2), URI_Map(m3), URI_Map(m4)) ⇒ URI_Map(m1 ++ m2 ++ m3 ++ m4)
          case (URI_Map(m1), URI_Map(m2), URI_Map(m3), URI_String(s)) ⇒ URI_Map(m1 ++ m2 ++ m3)
          case (URI_Map(m1), URI_Map(m2), URI_String(s), URI_Map(m4)) ⇒ URI_Map(m1 ++ m2 ++ m4)
          case (URI_Map(m1), URI_String(s), URI_Map(m3), URI_Map(m4)) ⇒ URI_Map(m1 ++ m3 ++ m4)
          case (URI_String(s), URI_Map(m2), URI_Map(m3), URI_Map(m4)) ⇒ URI_Map(m2 ++ m3 ++ m4)
          case (URI_Map(m1), URI_Map(m2), URI_String(s3), URI_String(s4)) ⇒ URI_Map(m1 ++ m2)
          case (URI_Map(m1), URI_String(s2), URI_Map(m3), URI_String(s4)) ⇒ URI_Map(m1 ++ m3)
          case (URI_String(s1), URI_Map(m2), URI_Map(m3), URI_String(s4)) ⇒ URI_Map(m2 ++ m3)
          case (URI_String(s1), URI_Map(m2), URI_String(s3), URI_Map(m4)) ⇒ URI_Map(m2 ++ m4)
          case (URI_Map(m1), URI_String(s2), URI_String(s3), URI_Map(m4)) ⇒ URI_Map(m1 ++ m4)
          case (URI_String(s1), URI_String(s2), URI_Map(m3), URI_Map(m4)) ⇒ URI_Map(m3 ++ m4)
          case (URI_Map(m1), URI_String(s2), URI_String(s3), URI_String(s4)) ⇒ URI_Map(m1)
          case (URI_String(s1), URI_Map(m2), URI_String(s3), URI_String(s4)) ⇒ URI_Map(m2)
          case (URI_String(s1), URI_String(s2), URI_Map(m3), URI_String(s4)) ⇒ URI_Map(m3)
          case (URI_String(s1), URI_String(s2), URI_String(s3), URI_Map(m4)) ⇒ URI_Map(m4)
        }): @unchecked) match {
          case URI_Map(m) ⇒ URI_Map(m ++ Map("uri_type" -> Left("absolute")))
        }
      case URI_Relative_Ref(relative_part, query, fragment) ⇒
        ((((eval(relative_part),
          query match {
            case Some(q) ⇒ eval(q)
            case None ⇒ URI_String("")
          },
          fragment match {
            case Some(f) ⇒ eval(f)
            case None ⇒ URI_String("")
          }): @unchecked) match {
          case (URI_Map(m1), URI_Map(m2), URI_Map(m3)) ⇒ URI_Map(m1 ++ m2 ++ m3)
          case (URI_Map(m1), URI_Map(m2), URI_String(s)) ⇒ URI_Map(m1 ++ m2)
          case (URI_Map(m1), URI_String(s), URI_Map(m3)) ⇒ URI_Map(m1 ++ m3)
          case (URI_String(s), URI_Map(m2), URI_Map(m3)) ⇒ URI_Map(m2 ++ m3)
          case (URI_Map(m1), URI_String(s2), URI_String(s3)) ⇒ URI_Map(m1)
          case (URI_String(s1), URI_Map(m2), URI_String(s3)) ⇒ URI_Map(m2)
          case (URI_String(s1), URI_String(s2), URI_Map(m3)) ⇒ URI_Map(m3)
        }): @unchecked) match {
          case URI_Map(m) ⇒ URI_Map(m ++ Map("uri_type" -> Left("relative")))
        }
      case URI_Relative_Part(authority, path) ⇒
        ((eval(authority), eval(path)): @unchecked) match {
          case (URI_Map(m1), URI_Map(m2)) ⇒ URI_Map(m1 ++ m2)
          case (URI_Map(m1), URI_String(s)) ⇒ URI_Map(m1)
          case (URI_String(s), URI_Map(m2)) ⇒ URI_Map(m2)
        }
      case URI_Relative_Part_Path(path) ⇒ eval(path)
      case URI_Scheme(s) ⇒ URI_Map(Map("scheme" -> Left(s)))
      case URI_Hier_Part_Absolute(authority, path) ⇒
        ((eval(authority),
          eval(path)): @unchecked) match {
          case (URI_Map(m1), URI_Map(m2)) ⇒ URI_Map(m1 ++ m2)
          case (URI_Map(m1), URI_String(s)) ⇒ URI_Map(m1)
          case (URI_String(s), URI_Map(m2)) ⇒ URI_Map(m2)
        }
      case URI_Hier_Part_Path(path) ⇒ eval(path)
      case URI_Reference(rule) ⇒ eval(rule)
      case URI_AbsoluteURI(scheme, hier_part, query) ⇒
        ((((eval(scheme),
          eval(hier_part),
          query match {
            case Some(q) ⇒ eval(q)
            case None ⇒ URI_String("")
          }): @unchecked) match {
          case (URI_Map(m1), URI_Map(m2), URI_Map(m3)) ⇒ URI_Map(m1 ++ m2 ++ m3)
          case (URI_Map(m1), URI_Map(m2), URI_String(s)) ⇒ URI_Map(m1 ++ m2)
          case (URI_Map(m1), URI_String(s), URI_Map(m3)) ⇒ URI_Map(m1 ++ m3)
          case (URI_String(s), URI_Map(m2), URI_Map(m3)) ⇒ URI_Map(m2 ++ m3)
          case (URI_Map(m1), URI_String(s2), URI_String(s3)) ⇒ URI_Map(m1)
          case (URI_String(s1), URI_Map(m2), URI_String(s3)) ⇒ URI_Map(m2)
          case (URI_String(s1), URI_String(s2), URI_Map(m3)) ⇒ URI_Map(m3)
        }): @unchecked) match {
          case URI_Map(m) ⇒ URI_Map(m ++ Map("uri_type" -> Left("absolute")))
        }
      case URI_Authority(userinfo, host, port) ⇒
        ((userinfo match {
          case Some(u) ⇒ eval(u)
          case None ⇒ URI_String("")
        }, eval(host), port match {
          case Some(p) ⇒ eval(p)
          case None ⇒ URI_String("")
        }): @unchecked) match {
          case (URI_Map(m1), URI_Map(m2), URI_Map(m3)) ⇒ URI_Map(m1 ++ m2 ++ m3 ++ Map("authority" -> m1("userinfo").left.map(_ + "@" + m2("host").left.get + ":" + m3("port").left.get)))
          case (URI_Map(m1), URI_Map(m2), URI_String(s)) ⇒ URI_Map(m1 ++ m2 ++ Map("authority" -> m1("userinfo").left.map(_ + "@" + m2("host").left.get)))
          case (URI_String(s), URI_Map(m2), URI_Map(m3)) ⇒ URI_Map(m2 ++ m3 ++ Map("authority" -> m2("host").left.map(_ + ":" + m3("port").left.get)))
          case (URI_String(s1), URI_Map(m2), URI_String(s3)) ⇒ URI_Map(m2 ++ Map("authority" -> m2("host")))
        }
      case URI_UserInfo(user, password) ⇒
        ((eval(user), password match {
          case Some(p) ⇒ eval(p)
          case None ⇒ URI_String("")
        }): @unchecked) match {
          case (URI_Map(m1), URI_Map(m2)) ⇒ URI_Map(m1 ++ m2 ++ Map("userinfo" -> m1("user").left.map(_ + ":" + m2("password").left.get)))
          case (URI_Map(m1), URI_String(s)) ⇒ URI_Map(m1 ++ Map("userinfo" -> m1("user")))
          case (URI_String(s), URI_Map(m2)) ⇒ URI_Map(m2 ++ Map("userinfo" -> m2("password")))
        }
      case URI_User(user) ⇒ URI_Map(Map("user" -> Left(user)))
      case URI_Password(password) ⇒ URI_Map(Map("password" -> Left(password)))
      case URI_Reg_Name(name) ⇒ URI_String(name)
      case URI_Port(port) ⇒ URI_Map(Map("port" -> Left(port)))
      case URI_Path(path) ⇒ eval(path)
      case URI_Path_AbEmpty(path) ⇒ URI_Map(Map("path" -> Left(path)))
      case URI_Path_Absolute(path_absolute) ⇒ URI_Map(Map("path" -> Left(path_absolute)))
      case URI_Path_NoScheme(path_noscheme) ⇒ URI_Map(Map("path" -> Left(path_noscheme)))
      case URI_Path_Rootless(path_rootless) ⇒ URI_Map(Map("path" -> Left(path_rootless)))
      case URI_Path_Empty(path_empty) ⇒ URI_Map(Map("path" -> Left(path_empty)))
      case URI_Host(rule) ⇒ (eval(rule): @unchecked) match {
        case URI_String(s) ⇒ URI_Map(Map("host" -> Left(s)))
      }
      case URI_IP_Literal(rule) ⇒ eval(rule)
      case URI_IPvFuture(ipvfuture) ⇒ URI_String(ipvfuture)
      case URI_IPv6Address(address) ⇒ URI_String(address)
      case URI_IPv4Address(address) ⇒ URI_String(address)
      //case URI_Query(query)                 ⇒ URI_Map(Map("query" -> query))
      case URI_Query(rule) ⇒ URI_Map(Map("params" -> traverseParameterList(rule, Nil)))
      case URI_QueryParameter(queryVariable, queryValue) ⇒
        ((eval(queryVariable), eval(queryValue)): @unchecked) match {
          case (URI_String(variable), URI_String(value)) ⇒ URI_Tuple((variable, value))
        }
      case URI_QueryVariable(queryVariable) ⇒ URI_String(queryVariable)
      case URI_QueryValue(queryValue) ⇒ URI_String(queryValue)
      case URI_QueryToken(queryToken) ⇒ URI_String(queryToken)
      case URI_Fragment(fragment) ⇒ URI_Map(Map("fragment" -> Left(fragment)))
      case Error(e) ⇒ URI_String("Error" + e)
    }
  }

  @tailrec
  private def traverseParameterList(l: Seq[URI_AST], params: List[(String, String)]): Either[String, List[(String, String)]] = l match {
    case Nil ⇒ Right(params)
    case x +: xs ⇒ (eval(x): @unchecked) match {
      case URI_Tuple((k, v)) ⇒ traverseParameterList(xs, params :::(k, v) :: Nil)
      case URI_String(t) ⇒ traverseParameterList(xs, params :::(t, "") :: Nil)
    }
  }
}

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

package org

import org.parboiled2.CharPredicate
import org.parboiled2.CharPredicate.AlphaNum

package object arktos {
  val arktos_version = "Arktos version 0.9"

  /* Surrogate treatment lent from discussion at
   https://gitter.im/sirthias/parboiled2?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge */
  val isHighSurrogate = CharPredicate.from(Character.isHighSurrogate)
  val isLowSurrogate = CharPredicate.from(Character.isLowSurrogate)

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

  val notEncoded = unreserved ++ reserved ++ query_delims
}

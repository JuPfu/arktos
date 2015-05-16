/*
* Copyright (C) 2014-2015 Juergen Pfundt
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


import org.parboiled2.{ ErrorFormatter, ParseError, ParserInput }

import org.arktos.URIParser.URI_Map
import org.arktos.GetCmdLineArgs._

import scala.util.{ Success, Failure }

object Main extends App {

  val cmdLineArgs = argsParser.parse(args, Config())

  if (cmdLineArgs == None) {
    sys.exit(1)
  }

  if (cmdLineArgs.get.version) {
    System.err.println("Arktos version 0.1")
    sys.exit(2)
  }

  val input_uri = cmdLineArgs.get.input_uri
  val validate = cmdLineArgs.get.validate
  val verbose = cmdLineArgs.get.verbose

  if (verbose) {
    System.err.println((if (!validate) "Analyse: " else "Validate: ") + input_uri)
    System.err.flush()
  }

  val ms: Double = System.currentTimeMillis

  lazy val input: ParserInput = input_uri

  val parser = new URIParser(input)
  val res = parser.URI_reference.run()

  res match {
    case Success(x) ⇒
      val m = ((new evalURI).eval(x): @unchecked) match { case URI_Map(x) ⇒ x }
      System.out.println("RESULT->" + m.mapValues { case Left(v) ⇒ v; case (Right(v)) ⇒ v })
    case Failure(e: ParseError) ⇒ System.err.println("Input '" + args(0) + "': " + parser.formatError(e, new ErrorFormatter(showTraces = true)))
    case Failure(e)             ⇒ System.err.println("Input '" + args(0) + "': Unexpected error during parsing run: " + e)
  }
}


/*
* Copyright (C) 2015-2016 Juergen Pfundt
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

import org.arktos.GetCmdLineArgs._

object Main extends App {

  val cmdLineArgs = argsParser.parse(args, Config())

  if (cmdLineArgs.isEmpty) {
    sys.exit(1)
  }

  if (cmdLineArgs.get.version) {
    System.err.println("Arktos version 0.1")
    sys.exit(2)
  }

  val input = cmdLineArgs.get.input_uri
  val validate = cmdLineArgs.get.validate
  val verbose = cmdLineArgs.get.verbose

  if (verbose) {
    System.err.println((if (!validate) "Analyse: " else "Validate: ") + input)
  }

  val parsedURI = URIParser(input)

  if (parsedURI.isFailure) {
    System.err.println("Input '" + input + "': " + parsedURI.failed.get)
  } else {
    System.out.println("URI=" + parsedURI)

    System.out.println("Result->" + parsedURI.get.mapValues { case Left(v) ⇒ v; case (Right(v)) ⇒ v })

    val uri = parsedURI.get

    System.out.println("Get keys " + uri.keys)
    System.out.println("Get values " + uri.values)
    System.out.println("Contains 'scheme' is " + uri.contains("scheme"))
    System.out.println("Get value for 'scheme' = " + uri.getOrElse("scheme", Left("https")))
    System.out.println("Parameter to array = " + uri.toParArray)
    System.out.println("Parameter array(1) = " + uri.toParArray(1)._2.left.get)
    System.out.println("Drop key and value '-' = " + (uri - "scheme"))
    System.out.println("Add key and value '+' = " + uri + ("scheme" -> Left("https")))

    System.out.println("Concatenation of uris = " + (uri ++ Map("jp" -> Right(List(("a", "b"))))))

    val params = uri.getOrElse("params", Right(List())).right.get
    System.out.println("List of sorted Params =" + params.sorted)
  }
}


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

    val uri = parsedURI.get

    System.out.println("Get keys " + uri.keys)
    System.out.println("Get values " + uri.values)
    val uscheme = uri("scheme")

    val usum = uscheme + "::" + uri("authority")
    System.out.println("usum = " + usum)
    System.out.println("scheme = " + uscheme)
    System.out.println("Contains 'scheme' is " + uri.contains("scheme"))
    System.out.println("Get value for 'scheme' = " + uri.getOrElse("scheme", "https"))
    System.out.println("Parameter to array = " + uri.toParArray)

    val uprot = uri("protocol")
    System.out.println("protocol = " + uprot)
    val utipar = uri.toParArray(1)._2
    System.out.println("Parameter array(1) = " + utipar)
    val udrop = uri - "scheme"
    System.out.println("Drop key and value '-' = " + udrop)
    val uadd = uri + ("scheme" → "https")
    System.out.println("Add key and value '+' = " + uadd)

    val uconcat = uri ++ Map("jp" → List(("a", "b")))
    System.out.println("Concatenation of uris = " + uconcat)

    val params: List[(String, String)] = uri("params").asInstanceOf[List[(String, String)]]

    System.out.println("List of sorted Params =" + params.sorted)

    //val first = params.head

    //System.out.println("First = "+ first)
  }
}


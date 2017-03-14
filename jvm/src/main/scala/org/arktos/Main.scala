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
import org.arktos.URI.URIType

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

  val fmt = cmdLineArgs.get.fmt

  val parsedURI = if ( fmt == "uri" ) URIParser(input) else IRIParser(input)

  if (parsedURI.isFailure) {
    System.err.println("Input '" + input + "': " + parsedURI.failed.get)
  } else {
    System.out.println("URI=" + parsedURI)

    val uri = parsedURI.get

    System.out.println("Get keys " + uri.keys)
    System.out.println("Get values " + uri.values)
    val uscheme = uri("scheme")

    System.out.println("authority = " + (uscheme + "::" + uri("authority")))
    System.out.println("scheme = " + uscheme)
    System.out.println("Contains 'scheme' is " + uri.contains("scheme"))
    System.out.println("Get value for 'scheme' = " + uri.getOrElse("scheme", "https"))
    System.out.println("Parameter to array = " + uri.toParArray)

    val prot = uri("protocol")
    System.out.println("protocol = " + prot)
    System.out.println("Parameter array(1) = " + (uri.toParArray(1)._2))
    System.out.println("Drop key and value '-' = " + (uri - "scheme"))
    System.out.println("Add key and value '+' = " + (uri + ("scheme" → "https")))
    System.out.println("Concatenation = " + (uri + ("jp" → List(("a", "b")))))

    val params = uri.getOrElse("params", List(("", ""))).asInstanceOf[List[(String, String)]]
    System.out.println("List of sorted Params =" + params.sorted)

    System.out.println("Update params = " + uri.updated("params", List(("a", "1"), ("z", "24"))))

    val x = (uri - "params" - "raw_params" + ("params" → List(("x", "u"), ("y", "v"))))("params")
    System.out.println("params new = " + x.asInstanceOf[List[(String, String)]])

    // set up a new URI
    //val newUriReturnValue = URIReturnValue.URIMap(Map("scheme" → "https"))

    //val newUri = newUriReturnValue match { case URIReturnValue.URIMap(x) ⇒ x }

    val encoder = new URIEncoder

    val uri1: URIType = URI.get +
      ("scheme" → "https") +
      ("path" → "/github.com/JuPfu/arktos/graphs/traffic") +
      ("params" → List(("user", "jp"), ("code", encoder.encode("TestA3ßÜ §1"))))

    System.out.println("uri1 = " + uri1)

    val uri_synthesized = URI.build(uri)

    System.out.println("URI synthesized = " + uri_synthesized)

    // check URI
    val checkURI = URIParser(uri_synthesized)
    if (checkURI.isFailure) {
      System.err.println("Input '" + input + "': " + checkURI.failed.get)
    } else {
      System.out.println("URI=" + checkURI)
    }
  }
}


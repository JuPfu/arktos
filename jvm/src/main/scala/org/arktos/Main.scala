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

import scala.util.{ Try, Success, Failure }

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

  // use "-f uri" or "-f iri" to select URI or IRI parsing
  val parsedURI = if (fmt == "uri") URIParser(input) else IRIParser(input)

  // inquire about the result of the parsing process
  if (parsedURI.isFailure) {
    System.err.println("Input '" + input + "': " + parsedURI.failed.get)
  } else {

    // this is delivered from the URI or IRI parser
    System.out.println("URI=" + parsedURI)

    // get the URI respectively IRI
    val uri = parsedURI.get

    // get all keys contained in the parsed URI in a Set
    val keys = uri.keys
    System.out.println("Keys of URI parsed " + keys)

    // iterate over keys
    uri.keys.foreach(key ⇒ System.out.println("key = " + key + "\t\tvalue = " + uri.getOrElse(key, "")))

    // show all values contained in the parsed URI respectively IRI
    System.out.println("Get values " + uri.values)

    // get some values
    System.out.println("authority = " + (if (uri.contains("autority")) uri("authority") else ""))
    // easier
    System.out.println("authority = " + uri.getOrElse("autority", ""))
    // check for key
    System.out.println("Contains 'scheme' is " + uri.contains("scheme"))

    val s = if (uri.contains("scheme")) uri("scheme") else "http"

    uri.get("scheme") match {
      case Some(s) ⇒ s // return value for key "scheme"
      case None    ⇒ "http" // use "http" as default
    }

    val t: Try[String] = Try { uri("schemes").toString }
    if (t.isFailure) System.err.println("KEY schemes not found")
    else System.err.println("Get value for SCHEME " + t.get)

    // get a value with definite fallback
    System.out.println("Get value for 'scheme' = " + uri.getOrElse("scheme", "https"))
    if (uri.contains("scheme")) {
      // update "scheme"
      System.out.println("Update scheme = " + (uri.updated("scheme", "ftp")))
      // update in a different way
      System.out.println("Update scheme via '-' and '+' and retrieve value for scheme = " + ((uri - "scheme" + "scheme" → "mailto"))("scheme"))
      // update in a different way -- last value succeeds
      System.out.println("Update scheme via '-' and '+' = " + ((uri - "scheme" + "scheme" → "mailto" + "scheme" → "gopher"))("scheme"))
      // drop key and value
      val uriWithoutScheme = uri - "scheme"
      System.out.println("Drop key and value = " + uriWithoutScheme)
      // add key and value
      System.out.println("Add key and value = " + (uriWithoutScheme + "scheme" → "ssh"))
    }

    // get params
    val params = uri.getOrElse("params", List(("p", "12345"))).asInstanceOf[List[(String, String)]]
    // print sorted params
    System.out.println("List of sorted Params =" + params.sorted)
    // add tuple at head of params List
    System.out.println("Add  tuple at head of param list = " + (("u", "$20") :: params))
    // update params
    System.out.println("Update params = " + uri.updated("params", List(("a", "1"), ("z", "24"))))

    // get segments
    val seg = uri.getOrElse("segment", List()).asInstanceOf[List[String]]

    System.out.println("Segments = " + seg)

    val p = (uri - "params" + ("params" → List(("x", "u"), ("y", "v"))))("params")
    System.out.println("params new = " + p.asInstanceOf[List[(String, String)]])

    // set up a new URI
    val encoder = URIEncoder()

    val pathParser = new URIParser("/github.com/JuPfu/arktos/graphs/traffic").path.run()
    if (pathParser.isFailure) System.err.println("Error in path: " + pathParser.get)
    else System.err.println("PATH is okay: " + pathParser.get)

    val validateP = URIParser.validatePath(uri.getOrElse("path", "").toString)
    if (validateP.isFailure) System.err.println("Error in path: " + validateP.get)
    else System.err.println("PATH is okay: " + uri.getOrElse("path", ""))

    val uri1 = URI.get + // get empty URI
      "scheme" → "https" +
      "path" → "/github.com/JuPfu/arktos/graphs/traffic" +
      "params" → List(("user", "jp"), ("code", encoder.encode("TestA3ßÜ §1")))

    System.out.println("uri1 = " + uri1)

    val fb = URIParser("http://JeffryJones:highschool@//www.example.com:8042/ferrari/california/spyder.html?who&surname=Bueller&firstname=Ferris&film=#Chicago")
    val fb_uri = if (fb.isSuccess) fb.get else URI.get // empty URI
    System.err.println("fb = " + fb_uri)

    val modified_uri = uri1 - "user" - "password" - "scheme"
    System.err.println("Modified uri = " + modified_uri)

    val fb_params = fb_uri.getOrElse("params", List()).asInstanceOf[List[(String, String)]]
    val fb_newParams = fb_params.foldLeft(List.empty[(String, String)])((x, y) ⇒ if (y._2 != "" && y._2 != null) x ++ List(y) else x)
    println("fb_params without singletons = " + fb_newParams.sorted)

    // build uri string
    val uri_synthesized = URI.build(uri1)

    System.out.println("URI synthesized = " + uri_synthesized)

    // check URI
    val checkURI = URIParser(uri_synthesized, true)
    if (checkURI.isFailure) {
      System.err.println("Input '" + input + "': " + checkURI.failed.get)
    } else {
      System.out.println("--->URI=" + checkURI)
    }
  }
}


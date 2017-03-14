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

import scopt._

object GetCmdLineArgs {

  case class Config(
    validate:  Boolean = false,
    verbose:   Boolean = false,
    version:   Boolean = false,
    fmt:       String  = "uri",
    trace:     Boolean = false,
    input_uri: String  = "jp"
  )

  val argsParser = new OptionParser[Config]("Arktos") {
    head("Arktos", "version 0.1")
    help("help") text "prints this usage text"
    opt[Unit]("verbose") action { (_, c) ⇒ c.copy(verbose = true) } text "give some additional information"
    opt[Unit]("version") action { (_, c) ⇒ c.copy(version = true) } text "Arktos version information"
    opt[Unit]('v', "validate") action { (_, c) ⇒ c.copy(validate = true) } text "validate input"
    opt[Unit]('t', "trace") action { (_, c) ⇒ c.copy(trace = true) } text "display error trace"
    opt[String]('f', "fmt").valueName("uri,iri").action((x, c) ⇒ c.copy(fmt = x.toLowerCase())).validate(f ⇒ if (f.equals("uri".toLowerCase) || f.equals("iri".toLowerCase)) success else failure(s"format $f not supported")) text ("URI or IRI")
    arg[String]("<URI>") required () action { (x, c) ⇒ c.copy(input_uri = x) } text "uri"
  }
}

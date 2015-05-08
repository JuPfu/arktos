**Arktos**  A Parboiled2 based Scala Parser for the RFC 3986 URI Generic Syntax

Introduction
============

*Arktos* (Άρκτος) is the greek word for bear. 

With *Arktos* you can
- validate the syntax of a Uniform Resource Identifier (URI)

Installation
============
	
Create a *Arktos* JAR with all dependencies
--------------------------------------------

The *sbt-assembly* plugin located at https://github.com/sbt/sbt-assembly is used to create a *Arktos* JAR containing all dependencies.
Move into the *Arktos* directory.
From the command line type 

    sbt assembly
	
This should generate an archive

    target/scala-2.11/arktos-assembly-x.x.x.jar

where x.x.x denotes the version information, e.g. arktos-assembly-0.1.0.jar.

Running *Arktos* from the command line
----------------------------------------

Conversion of URI is done with the command shown here: 

    scala -cp ./target/scala-2.11/arktos-assembly-0.1-SNAPSHOT.jar org.arktos.URIParser "http://jp:secret@www.ietf.org/rfc/rfc2396.txt?p=1&p=URI#content"
    

*Arktos* delivers a map with which contains the parsed segments of the URI.

    testing: http://jp:secret@www.ietf.org/rfc/rfc2396.txt?p=1&p=URI#content
    RESULT->Map(path -> /rfc/rfc2396.txt, hostname -> www.ietf.org, uri_type -> absolute, userinfo -> jp:secret, host -> www.ietf.org, params -> List((p,1), (p,URI)), fragment -> content, hash -> #content, scheme -> http, user -> jp, authority -> jp:secret@www.ietf.org, password -> secret)

Currently the keys listed below are supported. The URL *http://member:secret@www.ietf.org:123/rfc/rfc2396.txt?p=1&p=URI#content* is used as an example to show
the parsed components in the example column.

| key | value | example |
------|-------|----------
| scheme | the detected scheme | *http* |
| user | the user part of the userinfo subcomponent | *member* |
| password | the password part of the userinfo subcomponent | *secret* |
| userinfo | the complete userinfo subcomponent consisting of 'user'-part and 'password'-part separated by a colon | *member:secret* |
| hostname | the name of the host | *www.ietf.org* |
| port | the port number | *123* |
| host | host and port separated by a colon | *www.ietf.org:123* |
| authority | userinfo and host separated by a '@' | *member:secret@www.ietf.org:123* |
| path | the complete path segment | */rfc/rfc2396.txt* |
| query | the query string | *p=1&p=URI* |
| params | the list of query parameters | *List((p,1), (p,URI))* |
| fragment | the fragment identifier component | *content* |
| hash | the fragment with preceding '#' | *#content* |
| uri_type | 'absolute' or 'relative' URI | *absolute* or *relative* |
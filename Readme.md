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
Move into the *Arktos** directory.
From the command line type 

    sbt assembly
	
This should generate an archive

    target/scala-2.11/arktos-assembly-x.x.x.jar

where x.x.x denotes the version information, e.g. chelona-assembly-0.9.0.jar.

Running *Arktos* from the command line
----------------------------------------

Conversion of URI is done with the command shown here: 

    scala -cp ./target/scala-2.11/arktos-assembly-0.1-SNAPSHOT.jar org.arktos.URIParser "http://jp:secret@www.ietf.org/rfc/rfc2396.txt?p=1&p=URI#content"
    
The output generated should be

    testing: http://jp:secret@www.ietf.org/rfc/rfc2396.txt?p=1&p=URI#content
    RESULT->Map(path -> /rfc/rfc2396.txt, hostname -> www.ietf.org, uri_type -> absolute, userinfo -> jp:secret, host -> www.ietf.org, params -> List((p,1), (p,URI)), fragment -> content, hash -> #content, scheme -> http, user -> jp, authority -> jp:secret@www.ietf.org, password -> secret)

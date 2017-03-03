import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

import scalariform.formatter.preferences._
import com.typesafe.sbt.SbtScalariform
import com.typesafe.sbt.SbtScalariform.ScalariformKeys

val commonSettings = Seq(
  version := "0.3.0",
  scalaVersion := "2.12.1",
  name := "arktos",
  organization := "com.github.jupfu",
  homepage := Some(new URL("http://github.com/JuPfu/arktos")),
  description := "URI Parser",
  startYear := Some(2015),
  licenses := Seq("Apache 2" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  test in assembly := {},
  javacOptions ++= Seq(
    "-encoding", "UTF-8",
    "-source", "1.8",
    "-target", "1.8",
    "-Xlint:unchecked",
    "-Xlint:deprecation"),
  scalacOptions ++= List(
    "-encoding",
    "UTF-8",
    "-feature",
    "-unchecked",
    "-deprecation",
    "-Xlint",
    "-Ypartial-unification",
    "-opt:l:method",
    "-language:_",
    "-target:jvm-1.8"))

val formattingSettings = scalariformSettings ++ Seq(
  ScalariformKeys.preferences := ScalariformKeys.preferences.value
    .setPreference(RewriteArrowSymbols, true)
    .setPreference(AlignParameters, true)
    .setPreference(AlignSingleLineCaseStatements, true)
    .setPreference(DoubleIndentClassDeclaration, true)
    .setPreference(PreserveDanglingCloseParenthesis, true))

/*
scalatex.SbtPlugin.projectSettings

lazy val readme = scalatex.ScalatexReadme(
  projectId = "readme",
  wd = file(""),
  url = "https://github.com/lihaoyi/scalatex/tree/master",
  source = "Readme"
)
*/

/////////////////////// PROJECTS /////////////////////////

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

resolvers += Resolver.sonatypeRepo("public")

resolvers += Resolver.sonatypeRepo("releases")

resolvers += Resolver.sonatypeRepo("snapshots")

scalariformSettings

ScalariformKeys.preferences := ScalariformKeys.preferences.value
  .setPreference(RewriteArrowSymbols, true)
  .setPreference(AlignParameters, true)
  .setPreference(AlignSingleLineCaseStatements, true)
  .setPreference(DoubleIndentClassDeclaration, true)
  .setPreference(PreserveDanglingCloseParenthesis, true)

lazy val arktos = crossProject.in(file("."))
  .settings(
    libraryDependencies ++=
      Seq(
        "org.parboiled" %%% "parboiled" % "2.1.4",
        "com.chuusai" %%% "shapeless" % "2.3.2",
        "org.scalatest" %%% "scalatest" % "3.0.1" % Test
      )
  )
  .jvmSettings(
    libraryDependencies ++=
      Seq(
        "com.github.scopt" % "scopt_2.12" % "3.5.0"
      )
  )
  .jsSettings(
    libraryDependencies ++=
      Seq(
      )
  )
  .settings(commonSettings: _*)
  .settings(scalariformSettings: _*)
  .settings(formattingSettings: _*)
  .settings(publishingSettings: _*)

lazy val arktosJVM = arktos.jvm
lazy val arktosJS = arktos.js

lazy val root = project.in(file("."))
  .enablePlugins(ScalaJSPlugin)
  .aggregate(arktosJVM, arktosJS)
  .settings(commonSettings:_*)

/*
lazy val arktos = project.in(file("."))
  .settings(formattingSettings: _*)
  .settings(commonSettings: _*)
  .settings(libraryDependencies ++= Seq(parboiled2, scopt, scalaTest))
  */

/////////////////////// PUBLISH /////////////////////////

lazy val publishingSettings = Seq(
  publishMavenStyle := true,
  publishArtifact in Test := false,
  useGpg := false,
  useGpgAgent := false,
  sonatypeProfileName := "JuPfu",
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (version.value.trim.endsWith("SNAPSHOT"))
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  },
  pomIncludeRepository := { _ => false },
  pomExtra :=
    <scm>
      <connection>scm:git:github.com/jupfu/arktos</connection>
      <developerConnection>scm:git:git@github.com:jupfu/chelona.git</developerConnection>
      <url>github.com/jupfu/arktos</url>
    </scm>
      <developers>
        <developer>
          <id>JuPfu</id>
          <name>Jürgen Pfundt</name>
          <url>http://github.com/jupfu</url>
        </developer>
      </developers>)



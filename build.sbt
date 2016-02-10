import com.typesafe.sbt.SbtScalariform.ScalariformKeys
import scalariform.formatter.preferences._

val commonSettings = Seq(
  version := "0.2.0",
  scalaVersion := "2.11.7",
  name := "arktos",
  organization := "com.github.jupfu",
  homepage := Some(new URL("http://github.com/JuPfu/arktos")),
  description := "URI Parser",
  startYear := Some(2015),
  licenses := Seq("Apache 2" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  javacOptions ++= Seq(
    "-encoding", "UTF-8",
    "-source", "1.6",
    "-target", "1.6",
    "-Xlint:unchecked",
    "-Xlint:deprecation"),
  scalacOptions ++= List(
    "-encoding", "UTF-8",
    "-feature",
    "-unchecked",
    "-deprecation",
    "-Xlint",
    "-language:_",
    "-target:jvm-1.6"))

val formattingSettings = scalariformSettings ++ Seq(
  ScalariformKeys.preferences := ScalariformKeys.preferences.value
    .setPreference(RewriteArrowSymbols, true)
    .setPreference(AlignParameters, true)
    .setPreference(AlignSingleLineCaseStatements, true)
    .setPreference(DoubleIndentClassDeclaration, true)
    .setPreference(PreserveDanglingCloseParenthesis, true))

test in assembly := {}

scalatex.SbtPlugin.projectSettings

lazy val readme = scalatex.ScalatexReadme(
  projectId = "readme",
  wd = file(""),
  url = "https://github.com/lihaoyi/scalatex/tree/master",
  source = "Readme"
)
	
/////////////////////// DEPENDENCIES /////////////////////////

val parboiled2       = "org.parboiled"   %% "parboiled"        % "2.1.0"
//val shapeless        = "com.chuusai" %% "shapeless" % "2.2.5"
val scopt            = "com.github.scopt" %% "scopt" % "3.3.0"
val scalaTest        = "org.scalatest"   % "scalatest_2.11"    % "2.2.6" % "test"

/////////////////////// PROJECTS /////////////////////////

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

resolvers += Resolver.sonatypeRepo("public")


scalariformSettings

ScalariformKeys.preferences := ScalariformKeys.preferences.value
  .setPreference(RewriteArrowSymbols, true)
  .setPreference(AlignParameters, true)
  .setPreference(AlignSingleLineCaseStatements, true)
  .setPreference(DoubleIndentClassDeclaration, true)
  .setPreference(PreserveDanglingCloseParenthesis, true)

libraryDependencies ++= Seq(parboiled2, scopt, scalaTest)

lazy val arktos = project.in(file("."))
  .settings(formattingSettings: _*)
  .settings(commonSettings: _*)
  .settings(libraryDependencies ++= Seq(parboiled2, scopt, scalaTest))


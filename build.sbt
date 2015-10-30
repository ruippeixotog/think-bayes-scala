import scalariform.formatter.preferences._

name := "think-bayes"

organization := "net.ruippeixotog"

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.7"

resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

libraryDependencies ++= Seq(
  "com.github.wookietreiber" %% "scala-chart"   % "0.5.0",
  "nz.ac.waikato.cms.weka"    % "weka-stable"   % "3.6.13",
  "org.apache.commons"        % "commons-math3" % "3.5",
  "org.specs2"               %% "specs2-core"   % "3.6.5"    % "test")

scalariformSettings

ScalariformKeys.preferences := ScalariformKeys.preferences.value.
  setPreference(AlignParameters, true)

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-feature",
  "-language:implicitConversions",
  "-language:higherKinds")

initialCommands in console := """
  import thinkbayes._
  import thinkbayes.extensions.Plotting._
  import thinkbayes.extensions.Distributions._"""

publishTo <<= version { v =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

licenses := Seq("MIT License" -> url("http://www.opensource.org/licenses/mit-license.php"))

homepage := Some(url("https://github.com/ruippeixotog/think-bayes-scala"))

pomExtra :=
  <scm>
    <url>https://github.com/ruippeixotog/think-bayes-scala</url>
    <connection>scm:git:https://github.com/ruippeixotog/think-bayes-scala.git</connection>
  </scm>
    <developers>
      <developer>
        <id>ruippeixotog</id>
        <name>Rui Gonçalves</name>
        <url>http://ruippeixotog.net</url>
      </developer>
    </developers>

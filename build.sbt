name := "think-bayes"
organization := "net.ruippeixotog"
version := "1.0-SNAPSHOT"

scalaVersion := "2.12.15"
crossScalaVersions := Seq("2.12.15")

resolvers += "Scalaz Bintray Repo" at "https://dl.bintray.com/scalaz/releases"

libraryDependencies ++= Seq(
  "de.sciss"                 %% "scala-chart"   % "0.8.0",
  "nz.ac.waikato.cms.weka"    % "weka-stable"   % "3.8.6",
  "org.apache.commons"        % "commons-math3" % "3.6.1",
  "org.specs2"               %% "specs2-core"   % "4.15.0"    % "test")

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-feature",
  "-language:implicitConversions",
  "-language:higherKinds")

console / initialCommands := """
  import thinkbayes._
  import thinkbayes.extensions.Plotting._
  import thinkbayes.extensions.Distributions._"""

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishMavenStyle := true

Test / publishArtifact := false

pomIncludeRepository := { _ => false }

licenses := Seq("MIT License" -> url("http://www.opensource.org/licenses/mit-license.php"))

homepage := Some(url("https://github.com/ruippeixotog/think-bayes-scala"))

pomExtra := {
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
}

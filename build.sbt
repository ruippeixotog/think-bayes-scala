import scalariform.formatter.preferences._

name := "think-bayes"

organization := "net.ruippeixotog"

version := "0.2-SNAPSHOT"

scalaVersion := "2.11.4"

libraryDependencies ++= Seq(
  "com.github.wookietreiber" %% "scala-chart"   % "0.4.2",
  "nz.ac.waikato.cms.weka"    % "weka-stable"   % "3.6.11",
  "org.apache.commons"        % "commons-math3" % "3.3")

scalariformSettings

ScalariformKeys.preferences := ScalariformKeys.preferences.value.
  setPreference(AlignParameters, true)

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-feature",
  "-language:implicitConversions")

initialCommands in console := """
  import thinkbayes._
  import thinkbayes.extensions.Plotting._"""

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

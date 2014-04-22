name := "think-bayes"

scalaVersion := "2.10.3"

libraryDependencies ++= Seq(
  "com.github.wookietreiber" %% "scala-chart"   % "0.3.0",
  "nz.ac.waikato.cms.weka"    % "weka-stable"   % "3.6.6",
  "org.apache.commons"        % "commons-math3" % "3.2")

initialCommands in console := """
  import thinkbayes._
  import thinkbayes.examples._
  import thinkbayes.extensions.Plotting._"""

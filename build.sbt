name := "think-bayes"

scalaVersion := "2.11.4"

libraryDependencies ++= Seq(
  "com.github.wookietreiber" %% "scala-chart"   % "0.4.2",
  "nz.ac.waikato.cms.weka"    % "weka-stable"   % "3.6.11",
  "org.apache.commons"        % "commons-math3" % "3.3")

initialCommands in console := """
  import thinkbayes._
  import thinkbayes.examples._
  import thinkbayes.extensions.Plotting._"""

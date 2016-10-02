import com.lihaoyi.workbench.Plugin._

enablePlugins(ScalaJSPlugin)

workbenchSettings

name := "GameKit game"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom" % "0.9.0",
  "com.lihaoyi" %%% "scalatags" % "0.5.4",
  "org.scalacheck" %%% "scalacheck" % "1.13.2" % "test"
)

bootSnippet := """game.Main().main(document.getElementById("appRoot"));"""

refreshBrowsers <<= refreshBrowsers.triggeredBy(fastOptJS in Compile)


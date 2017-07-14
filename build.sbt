enablePlugins(WorkbenchPlugin)
enablePlugins(ScalaJSPlugin)
enablePlugins(ScalaJSBundlerPlugin)

name := "GameKit game"

version := "0.1-SNAPSHOT"

lazy val metaMacroSettings: Seq[Def.Setting[_]] = Seq(
  scalaVersion := "2.11.8",
  // New-style macro annotations are under active development.  As a result, in
  // this build we'll be referring to snapshot versions of both scala.meta and
  // macro paradise.
  resolvers += Resolver.sonatypeRepo("releases"),
  resolvers += Resolver.bintrayIvyRepo("scalameta", "maven"),
  // A dependency on macro paradise 3.x is required to both write and expand
  // new-style macros.  This is similar to how it works for old-style macro
  // annotations and a dependency on macro paradise 2.x.
  addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-beta4" cross CrossVersion.full),
  scalacOptions += "-Xplugin-require:macroparadise",
  scalacOptions += "-feature",
  scalacOptions += "-language:implicitConversions",
  // temporary workaround for https://github.com/scalameta/paradise/issues/10
  scalacOptions in (Compile, console) := Seq(), // macroparadise plugin doesn't work in repl yet.
  // temporary workaround for https://github.com/scalameta/paradise/issues/55
  sources in (Compile, doc) := Nil // macroparadise doesn't work with scaladoc yet.
)

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom" % "0.9.3",
  "org.scalacheck" %%% "scalacheck" % "1.13.2" % "test",
  "com.lihaoyi" %%% "upickle" % "0.4.3",
  "com.lihaoyi" %%% "scalatags" % "0.6.1"
)

jsDependencies in Compile += ProvidedJS / "cp.js"
jsDependencies in Compile += ProvidedJS / "d3-voronoi.js"
jsDependencies in Compile += ProvidedJS / "dat.gui.js"
jsDependencies in Compile += ProvidedJS / "clipper_unminified.js"

npmDependencies in Compile += "snabbdom" -> "0.5.3"
enableReloadWorkflow in Compile := true
emitSourceMaps := false

scalaJSUseMainModuleInitializer := true

mainClass in Compile := Some("pcgtest.PCGTest")

lazy val macros = project.in(file("macros"))
  .settings(metaMacroSettings)
  .settings(libraryDependencies += "org.scalameta" %% "scalameta" % "1.4.0")

metaMacroSettings
dependsOn(macros)

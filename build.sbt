enablePlugins(WorkbenchPlugin)
enablePlugins(ScalaJSPlugin)
enablePlugins(ScalaJSBundlerPlugin)

lazy val metaMacroSettings: Seq[Def.Setting[_]] = Seq(
  scalaVersion := "2.12.2",
  // New-style macro annotations are under active development.  As a result, in
  // this build we'll be referring to snapshot versions of both scala.meta and
  // macro paradise.
  resolvers += Resolver.sonatypeRepo("releases"),
  resolvers += Resolver.bintrayIvyRepo("scalameta", "maven"),
  // A dependency on macro paradise 3.x is required to both write and expand
  // new-style macros.  This is similar to how it works for old-style macro
  // annotations and a dependency on macro paradise 2.x.
  addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M9" cross CrossVersion.full),
  scalacOptions += "-Xplugin-require:macroparadise",
  scalacOptions += "-feature",
  scalacOptions += "-language:implicitConversions",
  // temporary workaround for https://github.com/scalameta/paradise/issues/10
  scalacOptions in (Compile, console) := Seq() // macroparadise plugin doesn't work in repl yet.
)

useYarn := true

lazy val commonSettings = Seq(
  organization := "net.nornagon",
  scalaVersion := "2.12.2",
  version := "0.1-SNAPSHOT",
  enableReloadWorkflow in Compile := true,
  emitSourceMaps := false,
  useYarn := true, // https://github.com/scalacenter/scalajs-bundler/issues/118
  libraryDependencies ++= Seq(
    "org.scala-js" %%% "scalajs-dom" % "0.9.3",
    "org.scalacheck" %%% "scalacheck" % "1.13.4" % "test",
    "com.lihaoyi" %%% "scalatags" % "0.6.2"
  )
)

lazy val snabbdom = project
  .settings(
    commonSettings,
    npmDependencies in Compile += "snabbdom" -> "0.5.3"
  )
  .enablePlugins(ScalaJSPlugin, ScalaJSBundlerPlugin)

lazy val kit = project
  .settings(
    commonSettings,
    metaMacroSettings,
    jsDependencies in Compile += ProvidedJS / "cp.js"
  )
  .dependsOn(macros)
  .enablePlugins(ScalaJSPlugin, ScalaJSBundlerPlugin)

lazy val game = project
  .settings(
    commonSettings,
    scalaJSUseMainModuleInitializer := true,
    mainClass in Compile := Some("game.Main")
  )
  .dependsOn(kit)
  .enablePlugins(WorkbenchPlugin, ScalaJSPlugin, ScalaJSBundlerPlugin)

lazy val macros = project.in(file("macros"))
  .settings(metaMacroSettings)
  .settings(libraryDependencies += "org.scalameta" %% "scalameta" % "1.8.0")

lazy val pcgtest = project
  .settings(
    commonSettings,
    metaMacroSettings,
    jsDependencies in Compile += ProvidedJS / "d3-voronoi.js",
    jsDependencies in Compile += ProvidedJS / "dat.gui.js",
    jsDependencies in Compile += ProvidedJS / "clipper_unminified.js",
    scalaJSUseMainModuleInitializer := true,
    mainClass in Compile := Some("pcgtest.PCGTest")
  )
  .dependsOn(kit, snabbdom, macros)
  .enablePlugins(WorkbenchPlugin, ScalaJSPlugin, ScalaJSBundlerPlugin)

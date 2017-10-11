name := "inferium"

scalaVersion := "2.12.3"


lazy val root = project.in(file(".")).
  aggregate(cli, web).
  settings(
    publish := {},
    publishLocal := {}
  )

lazy val core = crossProject.
  crossType(CrossType.Pure).
  in(file("core")).
  settings(
    name := "inferium-core",
    version := "0.1-SNAPSHOT"
  ).
  jvmSettings(
    // Add JVM-specific settings here
  ).
  jsSettings(
    // Add JS-specific settings here
  )

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val cli = project
  .in(file("cli"))
  .dependsOn(coreJVM)
  .settings(
    name := "inferium-cli",
    version := "0.1-SNAPSHOT",
    libraryDependencies += "com.lihaoyi" % "fastparse_2.12" % "0.4.4",
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % Test
  )


lazy val web = project
  .in(file("web"))
  .dependsOn(coreJS)
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "inferium-web",
    version := "0.1-SNAPSHOT",
    scalaJSUseMainModuleInitializer := true
  )
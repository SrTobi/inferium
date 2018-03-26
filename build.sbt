scalaVersion := "2.12.3"

lazy val commonSettings = Seq(
    organization := "de.srtobi",
    version := "0.1-SNAPSHOT",
    libraryDependencies += "com.lihaoyi" %% "upickle" % "0.5.1",
    libraryDependencies += "de.srtobi" %% "escalima" % "0.1",
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5" % Test,
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test
)

lazy val root = project.in(file(".")).
    aggregate(cli, web, coreJVM, coreJS).
    settings(
        name := "inferium",
        publish := {},
        publishLocal := {}
    )

lazy val core = crossProject.
    crossType(CrossType.Pure).
    in(file("core")).
    settings(commonSettings: _*).
    settings(
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
        commonSettings: _*
    )


lazy val web = project
    .in(file("web"))
    .enablePlugins(ScalaJSPlugin)
    .dependsOn(coreJS)
    .settings(commonSettings: _*)
    .settings(
        scalaJSUseMainModuleInitializer := false,
    )
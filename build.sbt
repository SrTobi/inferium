import org.scalajs.sbtplugin.cross

scalaVersion := "2.12.2"

lazy val commonSettings = Seq(
    organization := "de.srtobi",
    version := "0.1-SNAPSHOT",
    libraryDependencies += "de.srtobi" %%% "escalima" % "0.1",
    libraryDependencies += "com.lihaoyi" %%% "upickle" % "0.5.1",
    libraryDependencies += "com.lihaoyi" %%% "fastparse" % "1.0.0",
    libraryDependencies += "org.scalactic" %%% "scalactic" % "3.0.5" % Test,
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.5" % Test
)


lazy val root = project.in(file("."))
    .aggregate(cli, web, coreJVM, coreJS, testToolsJVM, testToolsJS, jsEvalJVM, jsEvalJS)
    .settings(
        name := "inferium",
        publish := {},
        publishLocal := {}
    )

lazy val core = crossProject
    .crossType(CrossType.Pure)
    .in(file("core"))
    .settings(commonSettings)

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val cli = project
    .in(file("cli"))
    .dependsOn(coreJVM)
    .settings(commonSettings)


lazy val web = project
    .in(file("web"))
    .enablePlugins(ScalaJSPlugin)
    .dependsOn(coreJS)
    .settings(
        scalaJSUseMainModuleInitializer := false,
    )
    .settings(commonSettings)

lazy val jsEval = crossProject
    .crossType(CrossType.Full)
    .in(file("extras/jseval"))
    .jvmSettings(
        fork := true
    )
    .settings(commonSettings)

lazy val jsEvalJVM = jsEval.jvm
lazy val jsEvalJS = jsEval.js

lazy val testTools = crossProject
    .crossType(CrossType.Pure)
    .in(file("extras/testtools"))
    .dependsOn(jsEval)
    .settings(commonSettings)

lazy val testToolsJVM = testTools.jvm
lazy val testToolsJS = testTools.js
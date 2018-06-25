
scalaVersion := "2.12.2"

//--------------------- common settings ---------------------//
lazy val commonSettings = Seq(
    organization := "de.srtobi",
    version := "0.1-SNAPSHOT",

    scalacOptions ++= Seq("-feature", "-Xfatal-warnings", "-deprecation", "-unchecked"),

    libraryDependencies += "de.srtobi" %%% "escalima" % "0.5",
    libraryDependencies += "com.lihaoyi" %%% "upickle" % "0.6.6",
    libraryDependencies += "com.lihaoyi" %%% "fastparse" % "1.0.0",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value,
    libraryDependencies += "org.scalactic" %%% "scalactic" % "3.0.5" % Test,
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.5" % Test,
    libraryDependencies += "org.scalacheck" %%% "scalacheck" % "1.13.4" % Test,

    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
)


//--------------------- task keys ---------------------//
lazy val updateResources = taskKey[Unit]("Updates resources of cross projects")
lazy val updateTests = taskKey[Unit]("Updates tests")
lazy val checkFixtures = taskKey[Unit]("Check if the test fixtures run when executed normally")


//--------------------- root ---------------------//
lazy val root = project.in(file("."))
    .aggregate(cli, web, coreJVM, coreJS, testToolsJVM, testToolsJS, jsEvalJVM, jsEvalJS, testCreator, macrosJVM, macrosJS, nodePreludeDataJVM, nodePreludeDataJS)
    .dependsOn(testCreator)
    .settings(
        name := "inferium",
        publish := {},
        publishLocal := {},
        fork := true
    )
    .settings(
        updateResources := {
            val base = baseDirectory.value / "extras/testtools"
            val main = base / "src/main"
            ResourceCompiler.bundle(main / "resources", main / "scala", "inferium.testtools", "Resources")
        }
    )
    .settings(
        updateTests := {
            val base = file("tests")
            val target = file("cli/src/test/scala")
            val pkg = "inferium.dataflow"
            (runMain in Compile).toTask(s" inferium.testcreator.cli.BundleFixtures ${base / "working"} $target $pkg WorkingFixturesSpec")
        }.value
    )
    .settings(
        checkFixtures := {
            val base = file("tests")
            (runMain in Compile).toTask(s" inferium.testcreator.cli.CheckFixtures $base")
        }.value
    )

//--------------------- macros ---------------------//
lazy val macros = crossProject
    .crossType(CrossType.Pure)
    .in(file("extras/macros"))
    .settings(commonSettings)
    .settings(
        libraryDependencies ++= Seq(
            "org.scala-lang" % "scala-reflect" % scalaVersion.value,
            "org.scala-lang" % "scala-compiler" % scalaVersion.value
        )
    )

lazy val macrosJVM = macros.jvm
lazy val macrosJS = macros.js

//--------------------- core ---------------------//
lazy val core = crossProject
    .crossType(CrossType.Pure)
    .in(file("core"))
    .dependsOn(macros)
    .settings(commonSettings)
    .jvmSettings(
        fork in Test := true
    )

lazy val coreJVM = core.jvm
lazy val coreJS = core.js


//--------------------- cli ---------------------//
lazy val cli = project
    .in(file("cli"))
    .dependsOn(coreJVM)
    .settings(commonSettings)
    .settings(
        fork in Test := true
    )


//--------------------- web ---------------------//
lazy val web = project
    .in(file("web"))
    .enablePlugins(ScalaJSPlugin)
    .enablePlugins(WorkbenchPlugin)
    .dependsOn(coreJS)
    .settings(commonSettings)
    .settings(
        scalaJSUseMainModuleInitializer := false,
        workbenchStartMode := WorkbenchStartModes.OnCompile,
        workbenchDefaultRootObject := Some(("web/index.html", "web/")),  // (defaultRootObject, rootDirectory)
        libraryDependencies += "com.thoughtworks.binding" %%% "dom" % "11.0.1"
    )


//--------------------- node prelude defs -------------//
lazy val nodePreludeData = crossProject
    .crossType(CrossType.Full)
    .in(file("extras/node-prelude"))
    .settings(commonSettings)
    .jvmSettings(
        fork := true
    )

lazy val nodePreludeDataJVM = nodePreludeData.jvm
lazy val nodePreludeDataJS = nodePreludeData.js

//--------------------- jsEval tools ---------------------//
lazy val jsEval = crossProject
    .crossType(CrossType.Full)
    .in(file("extras/jseval"))
    .jvmSettings(
        fork := true
    )
    .settings(commonSettings)

lazy val jsEvalJVM = jsEval.jvm
lazy val jsEvalJS = jsEval.js


//--------------------- test tools ---------------------//
lazy val testTools = crossProject
    .crossType(CrossType.Pure)
    .in(file("extras/testtools"))
    .dependsOn(jsEval)
    .settings(commonSettings)
    .jvmSettings(
        fork := true
    )

lazy val testToolsJVM = testTools.jvm
lazy val testToolsJS = testTools.js


//--------------------- test creator ---------------------//
lazy val testCreator = project
    .in(file("extras/testcreator"))
    .dependsOn(testToolsJVM, jsEvalJVM, coreJVM)
    .settings(commonSettings)
    .settings(
        fork := true,
        //baseDirectory in Test := file("./extras/testcreator")
    )

lazy val macroTest = project
    .in(file("extras/macros-test"))
    .dependsOn(macrosJVM)
    .settings(commonSettings)
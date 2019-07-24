lazy val V = new {
  val asm = "6.0"
  val scala = computeScalaVersionFromTravisYml("2.12")
  val scalafix = computeScalafixVersionFromBinScalafix()
  val scalameta = "4.1.4"
  val scalatest = "3.0.5"
}

addCommandAlias("ci-fmt", ui.ciFmt)
addCommandAlias("ci-fast", ui.ciFast)
addCommandAlias("ci-slow", ui.ciSlow)
addCommandAlias("ci-scalafix", ui.ciScalafix)
addCommandAlias("ci", ui.ci)
addCommandAlias("cleanAll", ui.cleanAll)
addCommandAlias("compileAll", ui.compileAll)
addCommandAlias("fmtAll", ui.fmtAll)
addCommandAlias("testAll", ui.testAll)
addCommandAlias("benchAll", ui.benchAll)
addCommandAlias("clean", ui.cleanAll)
addCommandAlias("compile", ui.compile)
addCommandAlias("fast", ui.fastTest)
addCommandAlias("slow", ui.slowTest)
addCommandAlias("fest", ui.scalafixTest)
addCommandAlias("test", ui.test)
addCommandAlias("fmt", ui.fmtAll)
addCommandAlias("benchParse", ui.benchParse)
addCommandAlias("benchIndex", ui.benchIndex)
addCommandAlias("benchOutline", ui.benchOutline)
addCommandAlias("benchSemanticdb", ui.benchSemanticdb)
addCommandAlias("benchScalasig", ui.benchScalasig)
addCommandAlias("benchScalac", ui.benchScalac)
addCommandAlias("publish", ui.publish)
addCommandAlias("publishLocal", ui.publishLocal)
addCommandAlias("publishSigned", ui.publishSigned)
addCommandAlias("rewrite", ui.rewrite)

version.in(ThisBuild) := {
  val rscVersion = version.in(ThisBuild).value.replace("+", "-")
  println(s"[info] Welcome to Rsc $rscVersion")
  val expectedJvmVersion = "1.8"
  val actualJvmVersion = sys.props("java.specification.version")
  if (actualJvmVersion != expectedJvmVersion) {
    val error = s"expected $expectedJvmVersion, actual $actualJvmVersion"
    sys.error(s"Unsupported JVM: $error")
  }
  rscVersion
}

lazy val bench = project
  .in(file("bench"))
  .dependsOn(tests)
  .enablePlugins(JmhPlugin)
  .settings(commonSettings)

lazy val check = project
  .in(file("check"))
  .dependsOn(rsc)
  .settings(
    commonSettings,
    publishableSettings,
    moduleName := "rsc-check",
    scalacOptions += "-Xexperimental",
    libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3",
    libraryDependencies += "commons-cli" % "commons-cli" % "1.4",
    libraryDependencies += "org.ow2.asm" % "asm" % V.asm,
    libraryDependencies += "org.ow2.asm" % "asm-util" % V.asm,
    libraryDependencies += "org.scala-lang" % "scalap" % V.scala,
    libraryDependencies += "org.eclipse.jgit" % "org.eclipse.jgit" % "4.6.0.201612231935-r",
    libraryDependencies += "io.github.java-diff-utils" % "java-diff-utils" % "4.0",
    libraryDependencies += "io.github.soc" % "directories" % "10",
    libraryDependencies += "org.scala-lang" % "scala-compiler" % V.scala,
    libraryDependencies += "org.scalameta" %% "cli" % V.scalameta,
    libraryDependencies += "org.scalameta" %% "metacp" % V.scalameta,
    libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.25",
    libraryDependencies += "org.slf4j" % "log4j-over-slf4j" % "1.7.25"
  )

lazy val examplesCore = project
  .in(file("examples/core"))
  .dependsOn(examplesFunction)
  .settings(
    commonSettings,
    semanticdbSettings,
    libraryDependencies += "org.scala-lang" % "scala-reflect" % V.scala,
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
  )

lazy val examplesDeps1 = project
  .in(file("examples/deps1"))
  .settings(commonSettings)

lazy val examplesDeps2 = project
  .in(file("examples/deps2"))
  .dependsOn(examplesDeps1)
  .settings(commonSettings)

lazy val examplesDeps3 = project
  .in(file("examples/deps3"))
  .dependsOn(examplesDeps2)
  .settings(commonSettings)

lazy val examplesError = project
  .in(file("examples/error"))
  .dependsOn(examplesErrordeps)
  .settings(commonSettings)

lazy val examplesErrordeps = project
  .in(file("examples/errordeps"))
  .settings(commonSettings)

lazy val examplesFunction = project
  .in(file("examples/function"))
  .settings(commonSettings)

lazy val examplesOriginalCore = project
  .in(file("examples/original/core"))
  .dependsOn(examplesFunction)
  .settings(
    commonSettings,
    semanticdbSettings,
    libraryDependencies += "org.scala-lang" % "scala-reflect" % V.scala,
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
  )

lazy val examplesSemantic = project
  .in(file("examples/semantic"))
  .dependsOn(examplesDeps3)
  .settings(commonSettings)

lazy val rsc = project
  .in(file("rsc"))
  .dependsOn(scalasig)
  .settings(
    commonSettings,
    publishableSettings,
    libraryDependencies += "org.scalameta" %% "semanticdb" % V.scalameta,
    libraryDependencies += "com.lihaoyi" %% "fastparse" % "1.0.0",
    mainClass := Some("rsc.cli.Main")
  )

lazy val scalafixInput = project
  .in(file("scalafix/input"))
  .settings(
    commonSettings,
    semanticdbSettings,
    libraryDependencies += "com.twitter" %% "bijection-core" % "0.9.5"
  )

lazy val scalafixOutput = project
  .in(file("scalafix/output"))
  .settings(
    commonSettings,
    semanticdbSettings,
    libraryDependencies += "com.twitter" %% "bijection-core" % "0.9.5"
  )

lazy val scalafixRules = project
  .in(file("scalafix/rules"))
  .dependsOn(rsc)
  .settings(
    commonSettings,
    publishableSettings,
    moduleName := "rsc-rules",
    libraryDependencies += "ch.epfl.scala" %% "scalafix-core" % V.scalafix
  )

lazy val scalafixTests = project
  .in(file("scalafix/tests"))
  .dependsOn(scalafixInput, scalafixRules)
  .enablePlugins(ScalafixTestkitPlugin)
  .settings(
    commonSettings,
    libraryDependencies += "ch.epfl.scala" % "scalafix-testkit" % V.scalafix % Test cross CrossVersion.full,
    scalafixTestkitOutputSourceDirectories :=
      sourceDirectories.in(scalafixOutput, Compile).value ++
        sourceDirectories.in(examplesCore, Compile).value,
    scalafixTestkitInputSourceDirectories :=
      sourceDirectories.in(scalafixInput, Compile).value ++
        sourceDirectories.in(examplesOriginalCore, Compile).value,
    scalafixTestkitInputClasspath :=
      fullClasspath.in(scalafixInput, Compile).value ++
        fullClasspath.in(examplesOriginalCore, Compile).value
  )

lazy val scalasig = project
  .in(file("scalasig/scalasig"))
  .settings(
    commonSettings,
    publishableSettings,
    libraryDependencies += "org.ow2.asm" % "asm" % V.asm,
    libraryDependencies += "org.ow2.asm" % "asm-tree" % V.asm
  )

lazy val scalap = project
  .in(file("scalasig/scalap"))
  .dependsOn(scalasig)
  .settings(
    commonSettings,
    publishableSettings,
    libraryDependencies += "org.scalameta" %% "cli" % V.scalameta,
    mainClass := Some("scala.meta.cli.Scalap")
  )

lazy val tests = project
  .in(file("tests"))
  .dependsOn(check, rsc, scalap)
  .enablePlugins(BuildInfoPlugin)
  .configs(Fast, Slow)
  .settings(
    commonSettings,
    libraryDependencies += "org.scalatest" %% "scalatest" % V.scalatest,
    libraryDependencies += "org.scalatest" %% "scalatest" % V.scalatest % "test",
    buildInfoUsePackageAsPath := true,
    buildInfoKeys := Seq(
      "sourceRoot" -> (baseDirectory in ThisBuild).value,
      BuildInfoKey.map(dependencyClasspath.in(examplesCore, Compile)) {
        case (k, v) => "coreClasspath" -> v.map(_.data)
      },
      BuildInfoKey.map(dependencyClasspath.in(examplesErrordeps, Compile)) {
        case (k, v) => "errorClasspath" -> v.map(_.data)
      },
      BuildInfoKey.map(dependencyClasspath.in(examplesFunction, Compile)) {
        case (k, v) => "functionClasspath" -> v.map(_.data)
      },
      BuildInfoKey.map(dependencyClasspath.in(examplesDeps1, Compile)) {
        case (k, v) => "depsClasspath" -> v.map(_.data)
      },
      BuildInfoKey.map(dependencyClasspath.in(examplesSemantic, Compile)) {
        case (k, v) => "semanticClasspath" -> v.map(_.data)
      }
    ),
    buildInfoPackage := "rsc.tests",
    testOptions.in(Test) += Tests.Argument("-l", "org.scalatest.tags.Slow"),
    inConfig(Fast)(Defaults.testTasks),
    inConfig(Slow)(Defaults.testTasks),
    testOptions.in(Slow) -= Tests.Argument("-l", "org.scalatest.tags.Slow"),
    testOptions.in(Slow) += Tests.Argument("-n", "org.scalatest.tags.Slow")
  )

lazy val commonSettings = Seq(
  organization := "com.twitter",
  scalaVersion := V.scala,
  crossScalaVersions := Seq(V.scala),
  scalacOptions ++= Seq("-Ypatmat-exhaust-depth", "off"),
  scalacOptions += "-deprecation",
  scalacOptions += "-unchecked",
  scalacOptions += "-feature",
  scalacOptions += "-Ywarn-unused-import",
  scalacOptions ++= { if (isCI) List("-Xfatal-warnings") else Nil },
  scalacOptions in (Compile, console) := Nil,
  fork in run := true,
  fork in Test := true,
  javaOptions += "-Xmx4G",
  baseDirectory in run := (baseDirectory in ThisBuild).value,
  baseDirectory in Test := (baseDirectory in ThisBuild).value,
  cancelable := true,
  resolvers += Opts.resolver.sonatypeReleases,
  resolvers += Opts.resolver.sonatypeSnapshots
)

lazy val publishableSettings = Seq(
  publishTo := Some {
    val prop = sys.props("repository")
    if (prop != null) "adhoc" at prop
    else if (version.value.endsWith("SNAPSHOT")) Opts.resolver.sonatypeSnapshots
    else Opts.resolver.sonatypeStaging
  },
  credentials ++= {
    val prop = sys.props("credentials")
    if (prop != null) List(new FileCredentials(file(prop)))
    else Nil
  },
  publishArtifact.in(Compile) := true,
  publishArtifact.in(Test) := false,
  publishMavenStyle := true,
  pomIncludeRepository := { x =>
    false
  },
  licenses += "Apache v2" -> url("https://github.com/twitter/rsc/blob/master/LICENSE.md"),
  pomExtra := (
    <url>https://github.com/twitter/rsc</url>
    <inceptionYear>2017</inceptionYear>
    <scm>
      <url>git://github.com/twitter/rsc.git</url>
      <connection>scm:git:git://github.com/twitter/rsc.git</connection>
    </scm>
    <issueManagement>
      <system>GitHub</system>
      <url>https://github.com/twitter/rsc/issues</url>
    </issueManagement>
    <developers>
      <developer>
        <id>xeno-by</id>
        <name>Eugene Burmako</name>
        <url>https://github.com/xeno-by</url>
      </developer>
      <developer>
        <id>ShaneDelmore</id>
        <name>Shane Delmore</name>
        <url>https://github.com/ShaneDelmore</url>
      </developer>
      <developer>
        <id>maxov</id>
        <name>Max Ovsiankin</name>
        <url>https://github.com/maxov</url>
      </developer>
      <developer>
        <id>WiWa</id>
        <name>Win Wang</name>
        <url>https://github.com/WiWa</url>
      </developer>
      <developer>
        <id>sundresh</id>
        <name>Sameer Sundresh</name>
        <url>https://github.com/sundresh</url>
      </developer>
    </developers>
  )
)

lazy val semanticdbSettings = Def.settings(
  scalacOptions -= "-deprecation",
  scalacOptions -= "-unchecked",
  scalacOptions -= "-feature",
  scalacOptions -= "-Ywarn-unused-import",
  scalacOptions -= "-Xfatal-warnings",
  addCompilerPlugin("org.scalameta" %% "semanticdb-scalac" % V.scalameta cross CrossVersion.full),
  scalacOptions += "-Yrangepos",
  scalacOptions += "-P:semanticdb:text:off",
  scalacOptions += "-P:semanticdb:symbols:all",
  scalacOptions += "-P:semanticdb:synthetics:on"
)

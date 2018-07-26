lazy val V = new {
  val asm = "6.0"
  val scala = computeScalaVersionFromTravisYml("2.11")
  val scalafix = computeScalafixVersionFromBinScalafix()
  val scalameta = "4.0.0-M7"
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
addCommandAlias("publishAll", ui.publishAll)
addCommandAlias("clean", ui.cleanAll)
addCommandAlias("compile", ui.compile)
addCommandAlias("fast", ui.fastTest)
addCommandAlias("slow", ui.slowTest)
addCommandAlias("fest", ui.scalafixTest)
addCommandAlias("test", ui.test)
addCommandAlias("fmt", ui.fmtAll)
addCommandAlias("benchParse", ui.benchParse)
addCommandAlias("benchLink", ui.benchLink)
addCommandAlias("benchOutline", ui.benchOutline)
addCommandAlias("benchSemanticdb", ui.benchSemanticdb)
addCommandAlias("benchMjar", ui.benchMjar)
addCommandAlias("publish", ui.publishAll)
addCommandAlias("publishLocal", ui.publishLocal)
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
  .dependsOn(mjar, rsc)
  .settings(
    commonSettings,
    publishableSettings,
    moduleName := "rsc-check",
    libraryDependencies += "com.googlecode.java-diff-utils" % "diffutils" % "1.3.0",
    libraryDependencies += "io.github.soc" % "directories" % "10",
    libraryDependencies += "org.scala-lang" % "scala-compiler" % V.scala,
    libraryDependencies += "org.scalameta" %% "metac" % V.scalameta cross CrossVersion.full,
    libraryDependencies += "org.scalameta" %% "metacp" % V.scalameta,
    libraryDependencies += "org.scalameta" %% "metai" % V.scalameta,
    libraryDependencies += "org.scalameta" %% "metap" % V.scalameta
  )

lazy val core = project
  .in(file("examples/core"))
  .dependsOn(function)
  .settings(
    commonSettings,
    semanticdbSettings,
    libraryDependencies += "org.scala-lang" % "scala-reflect" % V.scala,
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
  )

lazy val function = project
  .in(file("examples/function"))
  .settings(commonSettings)

lazy val mjar = project
  .in(file("mjar/mjar"))
  .dependsOn(scalasig)
  .disablePlugins(BackgroundRunPlugin)
  .settings(
    commonSettings,
    publishableSettings,
    libraryDependencies += "org.scalameta" %% "cli" % V.scalameta,
    libraryDependencies += "org.scalameta" %% "semanticdb" % V.scalameta,
    mainClass := Some("scala.meta.cli.Mjar")
  )

lazy val rsc = project
  .in(file("rsc"))
  .disablePlugins(BackgroundRunPlugin)
  .settings(
    commonSettings,
    publishableSettings,
    libraryDependencies += "org.scalameta" %% "semanticdb" % V.scalameta,
    mainClass := Some("rsc.cli.Main")
  )

lazy val scalafixInput = project
  .in(file("scalafix/input"))
  .settings(
    commonSettings,
    semanticdbSettings
  )

lazy val scalafixOutput = project
  .in(file("scalafix/output"))
  .settings(commonSettings)

lazy val scalafixRules = project
  .in(file("scalafix/rules"))
  .dependsOn(rsc)
  .settings(
    commonSettings,
    libraryDependencies += "com.github.xenoby" %% "scalafix-core" % V.scalafix
  )

lazy val scalafixTests = project
  .in(file("scalafix/tests"))
  .dependsOn(scalafixInput, scalafixRules)
  .settings(
    commonSettings,
    libraryDependencies += "com.github.xenoby" % "scalafix-testkit" % V.scalafix % Test cross CrossVersion.full
  )

lazy val scalasig = project
  .in(file("mjar/scalasig"))
  .settings(
    commonSettings,
    publishableSettings,
    libraryDependencies += "org.ow2.asm" % "asm" % V.asm,
    libraryDependencies += "org.ow2.asm" % "asm-tree" % V.asm
  )

lazy val scalap = project
  .in(file("mjar/scalap"))
  .dependsOn(scalasig)
  .disablePlugins(BackgroundRunPlugin)
  .settings(
    commonSettings,
    publishableSettings,
    libraryDependencies += "org.scalameta" %% "cli" % V.scalameta,
    mainClass := Some("scala.meta.cli.Scalap")
  )

lazy val tests = project
  .in(file("tests"))
  .dependsOn(check, function, mjar, rsc, scalap)
  .enablePlugins(BuildInfoPlugin)
  .configs(Fast, Slow)
  .settings(
    commonSettings,
    libraryDependencies += "org.scalatest" %% "scalatest" % V.scalatest,
    libraryDependencies += "org.scalatest" %% "scalatest" % V.scalatest % "test",
    buildInfoUsePackageAsPath := true,
    buildInfoKeys := Seq(
      "sourceRoot" -> (baseDirectory in ThisBuild).value,
      BuildInfoKey.map(dependencyClasspath.in(core, Compile)) {
        case (k, v) => "coreDeps" -> v.map(_.data)
      },
      BuildInfoKey.map(dependencyClasspath.in(function, Compile)) {
        case (k, v) => "functionDeps" -> v.map(_.data)
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
  licenses += "Apache v2" -> url(
    "https://github.com/twitter/rsc/blob/master/LICENSE.md"),
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
    </developers>
  )
)

lazy val semanticdbSettings = Def.settings(
  scalacOptions -= "-deprecation",
  scalacOptions -= "-unchecked",
  scalacOptions -= "-feature",
  scalacOptions -= "-Ywarn-unused-import",
  scalacOptions -= "-Xfatal-warnings",
  addCompilerPlugin(
    "org.scalameta" %% "semanticdb-scalac" % V.scalameta cross CrossVersion.full),
  scalacOptions += "-Yrangepos",
  scalacOptions += "-P:semanticdb:text:off",
  scalacOptions += "-P:semanticdb:symbols:on",
  scalacOptions += "-P:semanticdb:synthetics:off"
)

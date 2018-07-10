lazy val V = new {
  val asm = "6.0"
  val scala = computeScalaVersionFromTravisYml("2.11")
  val scalameta = "4.0.0-M4-143-94de8771-SNAPSHOT"
  val scalapb = _root_.scalapb.compiler.Version.scalapbVersion
  val scalatest = "3.0.5"
}

addCommandAlias("ci-fmt", ui.ciFmt)
addCommandAlias("ci-fast", ui.ciFast)
addCommandAlias("ci-slow", ui.ciSlow)
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
addCommandAlias("test", ui.test)
addCommandAlias("fmt", ui.fmtAll)
addCommandAlias("bench", ui.bench)
addCommandAlias("publish", ui.publishAll)
addCommandAlias("publishLocal", ui.publishLocal)

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

lazy val core = project
  .in(file("examples/core"))
  .dependsOn(function)
  .settings(
    commonSettings,
    scalacOptions -= "-deprecation",
    scalacOptions -= "-unchecked",
    scalacOptions -= "-feature",
    scalacOptions -= "-Ywarn-unused-import",
    scalacOptions -= "-Xfatal-warnings",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % V.scala,
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
    addCompilerPlugin(scalafixSemanticdb),
    scalacOptions += "-Yrangepos",
    scalacOptions += "-P:semanticdb:denotations:definitions",
    scalacOptions += "-P:semanticdb:mode:fat",
    scalacOptions += "-P:semanticdb:synthetics:none",
    addCommandAlias("rewrite", "core/scalafixCli --rules ExplicitResultTypes")
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
  .dependsOn(function, mjar, rsc, scalap)
  .enablePlugins(BuildInfoPlugin)
  .disablePlugins(BackgroundRunPlugin)
  .configs(Fast, Slow)
  .settings(
    commonSettings,
    libraryDependencies += "com.googlecode.java-diff-utils" % "diffutils" % "1.3.0",
    libraryDependencies += "org.scala-lang" % "scala-compiler" % V.scala,
    libraryDependencies += "org.scalameta" %% "metac" % V.scalameta cross CrossVersion.full,
    libraryDependencies += "org.scalameta" %% "metacp" % V.scalameta,
    libraryDependencies += "org.scalameta" %% "metap" % V.scalameta,
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
      },
      "mjarOut" -> classDirectory.in(mjar, Compile).value
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
    <inceptionYear>2018</inceptionYear>
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
        <url>http://xeno.by</url>
      </developer>
    </developers>
  )
)

lazy val protobufSettings = Def.settings(
  managedSourceDirectories in Compile += target.value / "protobuf-generated",
  PB.targets.in(Compile) := Seq(
    scalapb.gen(
      flatPackage = true // Don't append filename to package
    ) -> (target.value / "protobuf-generated")
  ),
  libraryDependencies += "com.thesamet.scalapb" %% "scalapb-runtime" % V.scalapb
)

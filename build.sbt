val V = new {
  val scala211 = "2.11.12"
  val scala212 = "2.12.4"
  val uTest = "0.6.0"
  val scalameta = computeScalametaVersionFromPluginsSbt()
}

addCommandAlias("benchAll", benchAll.command)
addCommandAlias("benchCI", benchCI.command)
addCommandAlias("benchQuick", benchQuick.command)
addCommandAlias("test", ";clean ;ci-all")
addCommandAlias("ci-all", ";ci-fmt ;ci-jvm ;ci-native")
addCommandAlias("ci-fmt", "scalafmtTest")
addCommandAlias("ci-jvm", "testsJVM/test")
addCommandAlias("ci-native", "testsNative/test")
lazy val isCI = sys.props.getOrElse("CI", default = "false") == "true"

lazy val commonSettings = Seq(
  organization := "com.twitter",
  version := version.value.replace("+", "-"),
  scalaVersion := V.scala211,
  crossScalaVersions := List(V.scala211, V.scala212),
  scalacOptions ++= Seq("-Ypatmat-exhaust-depth", "off"),
  scalacOptions += "-deprecation",
  scalacOptions += "-unchecked",
  scalacOptions += "-feature",
  scalacOptions += "-Ywarn-unused-import",
  scalacOptions ++= { if (isCI) List("-Xfatal-warnings") else Nil },
  scalacOptions in (Compile, console) := Nil,
  cancelable := true,
  publishArtifact in packageDoc := sys.env.contains("CI")
)

lazy val nativeSettings = Seq(
  nativeGC := "boehm",
  nativeMode := "release",
  nativeLinkStubs := true,
  crossScalaVersions := List(V.scala211)
)

lazy val benchJavac18 = project
  .in(file("bench/javac18"))
  .dependsOn(testsJVM)
  .enablePlugins(BuildInfoPlugin)
  .enablePlugins(JmhPlugin)
  .settings(commonSettings)

lazy val benchRsc = crossProject(JVMPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("bench/rsc"))
  .dependsOn(tests)
  .enablePlugins(BuildInfoPlugin)
  .enablePlugins(JmhPlugin)
  .settings(commonSettings)
  .jvmSettings(
    benchCliRscNative("Schedule"),
    benchCliRscNative("Typecheck")
  )
  .nativeSettings(nativeSettings)
lazy val benchRscJVM = benchRsc.jvm
lazy val benchRscNative = benchRsc.native

lazy val benchScalac211 = project
  .in(file("bench/scalac211"))
  .dependsOn(testsJVM)
  .enablePlugins(BuildInfoPlugin)
  .enablePlugins(JmhPlugin)
  .settings(
    commonSettings,
    scalaVersion := V.scala211,
    crossScalaVersions := List(V.scala211),
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value
  )

lazy val benchScalac212 = project
  .in(file("bench/scalac212"))
  .dependsOn(testsJVM)
  .enablePlugins(BuildInfoPlugin)
  .enablePlugins(JmhPlugin)
  .settings(
    commonSettings,
    scalaVersion := V.scala212,
    crossScalaVersions := List(V.scala212),
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value
  )

lazy val re2s = project
  .in(file("examples/re2s"))
  .settings(commonSettings)

lazy val rsc = crossProject(JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("rsc"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    commonSettings,
    libraryDependencies += "org.scalameta" %%% "semanticdb3" % V.scalameta,
    buildInfoPackage := "rsc.internal",
    buildInfoUsePackageAsPath := true,
    buildInfoKeys := Seq[BuildInfoKey](
      version
    )
  )
  .nativeSettings(nativeSettings)
lazy val rscJVM = rsc.jvm
lazy val rscNative = rsc.native

lazy val tests = crossProject(JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("tests"))
  .dependsOn(rsc)
  .enablePlugins(BuildInfoPlugin)
  .settings(
    commonSettings,
    libraryDependencies += "com.github.xenoby" %%% "utest" % V.uTest,
    libraryDependencies += "com.github.xenoby" %%% "utest" % V.uTest % "test",
    testFrameworks += new TestFramework("utest.runner.Framework"),
    buildInfoPackage := "rsc.tests",
    buildInfoUsePackageAsPath := true,
    buildInfoKeys := Seq[BuildInfoKey](
      "sourceRoot" -> (baseDirectory in ThisBuild).value,
      BuildInfoKey.map(stdlibClasspath) { case (k, v) => k -> v }
    )
  )
  .nativeSettings(
    nativeSettings,
    nativeMode := "debug"
  )
lazy val testsJVM = tests.jvm
lazy val testsNative = tests.native

lazy val root = project
  .in(file("."))
  .aggregate(rscJVM, rscNative, testsJVM, testsNative)
  .settings(commonSettings)

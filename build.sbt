val versions = new {
  val scala211 = "2.11.12"
  val scala212 = "2.12.4"
  val uTest = "0.6.0"
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
  organization := "org.twitter",
  version := version.value.replace("+", "-"),
  scalaVersion := versions.scala211,
  scalacOptions ++= Seq("-Ypatmat-exhaust-depth", "off"),
  scalacOptions += "-deprecation",
  scalacOptions += "-unchecked",
  scalacOptions += "-feature",
  scalacOptions += "-Ywarn-unused-import",
  scalacOptions ++= { if (isCI) List("-Xfatal-warnings") else Nil },
  scalacOptions in (Compile, console) := Nil,
  cancelable := true
)

lazy val benchSettings = commonSettings ++ Seq(
  buildInfoPackage := "rsc.bench",
  buildInfoUsePackageAsPath := true,
  buildInfoKeys := Seq[BuildInfoKey](
    "sourceRoot" -> (baseDirectory in ThisBuild).value
  )
)

lazy val nativeSettings = Seq(
  nativeGC := "immix",
  nativeMode := "release",
  nativeLinkStubs := true
)

lazy val benchJavac18 = project
  .in(file("bench/javac18"))
  .enablePlugins(BuildInfoPlugin)
  .enablePlugins(JmhPlugin)
  .settings(benchSettings)

lazy val benchRsc = crossProject(JVMPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("bench/rsc"))
  .dependsOn(rsc)
  .enablePlugins(BuildInfoPlugin)
  .enablePlugins(JmhPlugin)
  .jvmSettings(
    benchCliRscNative("Schedule"),
    benchCliRscNative("Typecheck")
  )
  .nativeSettings(nativeSettings)
  .settings(benchSettings)
lazy val benchRscJVM = benchRsc.jvm
lazy val benchRscNative = benchRsc.native

lazy val benchScalac211 = project
  .in(file("bench/scalac211"))
  .enablePlugins(BuildInfoPlugin)
  .enablePlugins(JmhPlugin)
  .settings(
    benchSettings,
    scalaVersion := versions.scala211,
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value
  )

lazy val benchScalac212 = project
  .in(file("bench/scalac212"))
  .enablePlugins(BuildInfoPlugin)
  .enablePlugins(JmhPlugin)
  .settings(
    benchSettings,
    scalaVersion := versions.scala212,
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value
  )

lazy val re2s = project
  .in(file("examples/re2s"))
  .settings(commonSettings)

lazy val rsc = crossProject(JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("rsc"))
  .enablePlugins(BuildInfoPlugin)
  .nativeSettings(nativeSettings)
  .settings(
    commonSettings,
    buildInfoPackage := "rsc.internal",
    buildInfoUsePackageAsPath := true,
    buildInfoKeys := Seq[BuildInfoKey](
      version
    )
  )
lazy val rscJVM = rsc.jvm
lazy val rscNative = rsc.native

lazy val tests = crossProject(JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("tests"))
  .dependsOn(rsc, benchRsc)
  .jvmSettings(
    libraryDependencies += "com.lihaoyi" %% "utest" % versions.uTest,
    libraryDependencies += "com.lihaoyi" %% "utest" % versions.uTest % "test"
  )
  .nativeSettings(
    nativeSettings,
    nativeMode := "debug",
    libraryDependencies += "com.lihaoyi" %%% "utest" % versions.uTest,
    libraryDependencies += "com.lihaoyi" %%% "utest" % versions.uTest % "test"
  )
  .settings(
    commonSettings,
    testFrameworks += new TestFramework("utest.runner.Framework")
  )
lazy val testsJVM = tests.jvm
lazy val testsNative = tests.native

lazy val root = project
  .in(file("."))
  .aggregate(rscJVM, rscNative, testsJVM, testsNative)
  .settings(commonSettings)

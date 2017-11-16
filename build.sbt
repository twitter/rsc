val versions = new {
  val scala = "2.11.11"
  val uTest = "0.6.0"
}

addCommandAlias("bench", s";clean ;${nightly.benches}")
addCommandAlias("ci", ";scalafmtTest ;clean ;testsJVM/test ;testsNative/test")
lazy val isCI = sys.props.getOrElse("CI", default = "false") == "true"

lazy val commonSettings = Seq(
  organization := "org.twitter",
  version := version.value.replace("+", "-"),
  scalaVersion := versions.scala,
  scalacOptions ++= Seq("-Ypatmat-exhaust-depth", "off"),
  scalacOptions += "-deprecation",
  scalacOptions += "-unchecked",
  scalacOptions += "-feature",
  scalacOptions += "-Ywarn-unused-import",
  scalacOptions ++= { if (isCI) List("-Xfatal-warnings") else Nil },
  scalacOptions in (Compile, console) := Nil,
  cancelable := true
)

lazy val bench = crossProject(JVMPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("bench"))
  .dependsOn(rsc, tests)
  .enablePlugins(JmhPlugin)
  .enablePlugins(BuildInfoPlugin)
  .jvmSettings(
    libraryDependencies += "org.scala-lang" % "scala-compiler" % versions.scala,
    benchCliRscNative("Typecheck"),
    benchCliRsc("Typecheck"),
    benchCliScalac("Typecheck"),
    benchCliScalac("Compile"),
    benchCliJavac("Compile")
  )
  .settings(
    commonSettings,
    buildInfoPackage := "rsc.bench",
    buildInfoUsePackageAsPath := true,
    buildInfoKeys := Seq[BuildInfoKey](
      "sourceRoot" -> (baseDirectory in ThisBuild).value
    )
  )
lazy val benchJVM = bench.jvm
lazy val benchNative = bench.native

lazy val re2s = project
  .in(file("examples/re2s"))
  .settings(commonSettings)

lazy val rsc = crossProject(JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("rsc"))
  .enablePlugins(BuildInfoPlugin)
  .nativeSettings(
    nativeLinkStubs := true,
    nativeGC := "immix"
  )
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
  .dependsOn(rsc)
  .enablePlugins(BuildInfoPlugin)
  .jvmSettings(
    libraryDependencies += "com.lihaoyi" %% "utest" % versions.uTest,
    libraryDependencies += "com.lihaoyi" %% "utest" % versions.uTest % "test"
  )
  .nativeSettings(
    libraryDependencies += "com.lihaoyi" %%% "utest" % versions.uTest,
    libraryDependencies += "com.lihaoyi" %%% "utest" % versions.uTest % "test",
    nativeLinkStubs := true,
    nativeGC := "immix"
  )
  .settings(
    commonSettings,
    testFrameworks += new TestFramework("utest.runner.Framework"),
    buildInfoPackage := "rsc.tests",
    buildInfoUsePackageAsPath := true,
    buildInfoKeys := Seq[BuildInfoKey](
      "sourceRoot" -> (baseDirectory in ThisBuild).value
    )
  )
lazy val testsJVM = tests.jvm
lazy val testsNative = tests.native

lazy val root = project
  .in(file("."))
  .aggregate(rscJVM, testsJVM, benchJVM)
  .settings(commonSettings)

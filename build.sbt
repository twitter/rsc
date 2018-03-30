addCommandAlias("benchAll", ui.benchAll)
addCommandAlias("benchCI", ui.benchCI)
addCommandAlias("benchQuick", ui.benchQuick)
addCommandAlias("ci-fmt", ui.ciFmt)
addCommandAlias("ci-jvm", ui.ciJvm)
addCommandAlias("ci-native", ui.ciNative)
addCommandAlias("test", ui.test)
addCommandAlias("publishLocal", ui.publishLocal)
addCommandAlias("publishSigned", ui.publishSigned)

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

lazy val commonSettings = Seq(
  organization := "com.twitter",
  scalaVersion := V.scala211,
  crossScalaVersions := List(V.scala211, V.scala212),
  scalacOptions ++= Seq("-Ypatmat-exhaust-depth", "off"),
  scalacOptions += "-deprecation",
  scalacOptions += "-unchecked",
  scalacOptions += "-feature",
  scalacOptions += "-Ywarn-unused-import",
  scalacOptions ++= { if (isCI) List("-Xfatal-warnings") else Nil },
  scalacOptions in (Compile, console) := Nil,
  cancelable := true
)

lazy val publishableSettings = Seq(
  credentials ++= {
    val prop = sys.props("credentials")
    if (prop != null) List(new FileCredentials(file(prop)))
    else Nil
  },
  publishTo := Some {
    val prop = sys.props("repository")
    if (prop != null) "adhoc" at prop
    else Opts.resolver.sonatypeStaging
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
        <url>http://xeno.by</url>
      </developer>
    </developers>
  )
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
    benchCliRscNative("Scan"),
    benchCliRscNative("Parse"),
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
    publishableSettings,
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
    libraryDependencies += "com.lihaoyi" %%% "utest" % V.uTest,
    libraryDependencies += "com.lihaoyi" %%% "utest" % V.uTest % "test",
    testFrameworks += new TestFramework("utest.runner.Framework"),
    fork in Test := true,
    javaOptions in Test += "-Xmx4G",
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

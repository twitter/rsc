addSbtPlugin("io.get-coursier" % "sbt-coursier" % "1.0.0-RC10")
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.2.25")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.7.0")
addSbtPlugin("com.dwijnand" % "sbt-dynver" % "2.0.0")
addSbtPlugin("org.scala-native" % "sbt-crossproject" % "0.2.2")
// This build is published from my private fork of Scala Native
// https://github.com/xeno-by/scala-native/commits/topic/scalameta
addSbtPlugin("com.github.xenoby" %% "sbt-scala-native" % "0.3.6-20-g0afae98f36")

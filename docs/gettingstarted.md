<!-- Copyright (c) 2017 Twitter, Inc. -->
<!-- Licensed under the Apache License, Version 2.0 (see LICENSE.md). -->

# Getting started

## Environment setup

Rsc has the following build dependencies:
  * Java 8
  * sbt 0.13.16

In addition to being compiled into JVM bytecode, Rsc is compiled with Scala
Native. If you'd like to compile native binaries or run native tests, check out
Scala Native documentation for
[additional instructions](http://www.scala-native.org/en/latest/user/setup.html).
When installing Scala Native install the optional `bdw-gc` and `re2` packages, as they
are required by the build.

At the moment, IntelliJ doesn't work well on the Rsc codebase. After opening
our project in IntelliJ, you may experience incomplete code intelligence,
spurious red squiggles and other unpleasant issues. We believe that this
is the case because of insufficient support for sbt-crossproject that we use
to crosscompile Rsc to both JVM and Native. We haven't yet found a good
workaround for this problem.

## Playing with Rsc

Right now, Rsc is not supposed to be usable in real-world Scala projects.
We only support [a small subset of Scala](language.md), and
[our typechecker](compiler.md) is just a prototype.
Nonetheless, this prototype [has already been very useful in studying compilation
performance](performance.md).

```
$ sbt
[info] Loading project definition from ~/Projects/rsc/project
[info] Updating {file:~/Projects/rsc/project/}rsc-build...
[info] Resolving org.scala-sbt.ivy#ivy;2.3.0-sbt-48dd0744422128446aee9ac31aa356ee203cc9f4
[info] Resolving org.fusesource.jansi#jansi;1.4 ...
[info] Done updating.
[info] Set current project to root (in build file:~/Projects/rsc/)
>
```

The best way to get your feet wet is to run our typechecker on an example
codebase. Feel free to change a thing or two to see how everything works.
Then go ahead and run `testsJVM/test-only rsc.typecheck.Re2sTests`.

```
> testsJVM/test-only rsc.typecheck.Re2sTests
-------------------------------- Running Tests --------------------------------
+ rsc.typecheck.Re2sTests.typecheck re2s 370ms
Tests: 1, Passed: 1, Failed: 0
```

The ultimate goal of our project is to achieve dramatic improvements
in compilation performance. Therefore, we are trying to be extremely careful
about measuring carefully and responsibly. To that end, we have set up
an automated benchmark suite that can be invoked via `bench`.

```
> bench
[info] Linking (1002 ms)
[info] Discovered 2089 classes and 15211 methods
[info] Optimizing (1301 ms)
[info] Generating intermediate code (432 ms)
[info] Produced 54 files
[info] Compiling to native code (1149 ms)
[info] Linking native code (233 ms)
[info] Running rsc.bench.CliRscNativeTypecheck ~/Projects/rsc/bench/native/target/scala-2.11/benchnative-out
...
[info] Running org.openjdk.jmh.Main ColdRscTypecheck HotRscTypecheck ColdScalacTypecheck HotScalacTypecheck
ColdScalacCompile HotScalacCompile ColdJavacCompile HotScalacCompile
[info] # JMH version: 1.19
[info] # VM version: JDK 1.8.0_111, VM 25.111-b14
[info] # VM invoker: /Library/Java/JavaVirtualMachines/jdk1.8.0_111.jdk/Contents/Home/jre/bin/java
[info] # VM options: -Xms2G -Xmx2G
...
```

## Contributing to Rsc

[Link](contributing.md)

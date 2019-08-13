<!-- Copyright (c) 2017-2019 Twitter, Inc. -->
<!-- Licensed under the Apache License, Version 2.0 (see LICENSE.md). -->

# Getting started

## Environment setup

Rsc has the following build dependencies:
  * Java 8
  * sbt 1.2.8

## Playing with Rsc

Right now, Rsc is not supposed to be usable in real-world Scala projects.
We only support [a subset of Scala](language.md), and
[our outliner](compiler.md) is just a prototype.
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

The best way to get your feet wet is to run our outliner on an example
codebase. Feel free to change a thing or two to see how everything works.
Then go ahead and run `tests/testOnly rsc.tests.SymbolTests`.

```
> tests/testOnly rsc.tests.SemanticdbTests
-------------------------------- Running Tests --------------------------------
[info] SemanticdbTests:
[info] - semanticdb for core
[info] Run completed in 3 seconds, 979 milliseconds.
[info] Total number of tests run: 1
[info] Suites: completed 1, aborted 0
[info] Tests: succeeded 1, failed 0, canceled 0, ignored 0, pending 0
[info] All tests passed.
[success] Total time: 4 s, completed Jul 2, 2018 12:49:36 PM
```

The ultimate goal of our project is to achieve dramatic improvements
in compilation performance. Therefore, we are trying to be extremely careful
about measuring carefully and responsibly. To that end, we have set up
an automated benchmark suite that can be invoked via `bin/bench`.

```
$ bin/bench
...
```

## Contributing to Rsc

[Link](contributing.md)
